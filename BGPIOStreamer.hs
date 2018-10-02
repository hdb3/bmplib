{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified System.Environment
import qualified Data.IP as IP
import qualified Text.Read
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as NS
import qualified System.IO
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams

import BMPMessage
import BGPlib hiding (BGPByteString,TLV,getBGPByteString)

main = do
    args <- System.Environment.getArgs
  
    source <- if null args
              then
                  return Streams.stdin
              else do let s = args !! 0
                          ip = IP.toHostAddress (Text.Read.read s :: IP.IPv4)
                      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
                      NS.connect sock ( NS.SockAddrInet 5000 ip)
                      handle <- NS.socketToHandle sock System.IO.ReadWriteMode
                      Streams.handleToInputStream handle

    stream <- Streams.parserToInputStream bmpParser source
    loop stream where
    loop stream = do
        msg <- Streams.read stream
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
fromBGP (BGPByteString bs) = Binary.decode $ L.fromStrict bs
