{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Binary

import BMPMessage
import BGPlib hiding (BGPByteString,TLV,getBGPByteString)

main = go bmpParser action

action msg = do
    putStrLn $ showBGPMsg msg

showBGPMsg :: BMPMsg -> String
showBGPMsg (BMPPeerUP x@BMPPeerUPMsg{..}) = show x ++ showBGP sentOpen ++ showBGP receivedOpen 
showBGPMsg (BMPRouteMonitoring ( RouteMonitoring perPeerHeader bGPMessage)) = "BMPRouteMonitoring { " ++ show perPeerHeader ++ showBGP bGPMessage ++ " }"
showBGPMsg x = show x


showBGP :: BGPByteString -> String
-- showBGP (BGPByteString bs) = " [ " ++ toHex bs ++ " ] "
showBGP = show . fromBGP

fromBGP :: BGPByteString -> BGPMessage
fromBGP (BGPByteString bs) = decode $ L.fromStrict bs

go parser action = do

    stream <- parserToInputStream parser stdin
    loop stream where
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do action bmpMsg
                              loop stream )
              msg


