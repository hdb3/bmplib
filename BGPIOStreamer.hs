{-# LANGUAGE RecordWildCards #-}
module Main where
import System.Environment
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Binary

import qualified Network.Socket as NS
import qualified System.IO
import Data.IP
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec.ByteString
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString
import Text.Read
import Control.Concurrent
import Control.Monad (forever)
import BMPMessage
import BGPlib hiding (BGPByteString,TLV,getBGPByteString)

main = do
    args <- getArgs
  
    source <- if null args
              then
                  return stdin
              else do let s = args !! 0
                          ip = toHostAddress (Text.Read.read s :: IPv4)
                      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
                      NS.connect sock ( NS.SockAddrInet 5000 ip)
                      handle <- NS.socketToHandle sock System.IO.ReadWriteMode
                      Streams.handleToInputStream handle

    stream <- parserToInputStream bmpParser source
    loop stream where
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do action bmpMsg
                              loop stream )
              msg


action msg = do
    putStrLn $ showBMPMsg msg

showBMPMsg :: BMPMsg -> String
showBMPMsg (BMPPeerUP x@BMPPeerUPMsg{..}) = show x ++ showBGPByteString sentOpen ++ showBGPByteString receivedOpen 
showBMPMsg (BMPRouteMonitoring ( RouteMonitoring perPeerHeader bGPMessage)) = "BMPRouteMonitoring { " ++ show perPeerHeader ++ showBGPByteString bGPMessage ++ " }"
showBMPMsg x = show x


showBGPByteString :: BGPByteString -> String
showBGPByteString = showBGP . fromBGP
showBGP BGPUpdate {..} = " BGPUpdate: "
                    ++ "\nNLRI:       " ++ show ( decodeAddrRange nlri )
                    ++ "\nWithdrawn:  " ++ show ( decodeAddrRange withdrawn )
                    ++ "\nAttributes: " ++ show ( decodeAttributes attributes )

showBGP x = show x

fromBGP :: BGPByteString -> BGPMessage
fromBGP (BGPByteString bs) = decode $ L.fromStrict bs
