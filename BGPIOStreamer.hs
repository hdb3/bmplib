{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Binary

import BMPMessage
import BGPlib hiding (BGPByteString,TLV,getBGPByteString)

main = go bmpParser action

action msg = do
    putStrLn $ showBMPMsg msg

showBMPMsg :: BMPMsg -> String
showBMPMsg (BMPPeerUP x@BMPPeerUPMsg{..}) = show x ++ showBGPByteString sentOpen ++ showBGPByteString receivedOpen 
showBMPMsg (BMPRouteMonitoring ( RouteMonitoring perPeerHeader bGPMessage)) = "BMPRouteMonitoring { " ++ show perPeerHeader ++ showBGPByteString bGPMessage ++ " }"
showBMPMsg x = show x


showBGPByteString :: BGPByteString -> String
showBGPByteString = showBGP . fromBGP
showBGP BGPUpdate {..} = " BGPUpdate: "
                    ++ "\nNLRI:       " ++ show nlri'
                    ++ "\nWithdrawn:  " ++ show withdrawn'
                    ++ "\nAttributes: " ++ show attributes' where
    nlri' = decodeAddrRange nlri
    withdrawn' = decodeAddrRange withdrawn
    attributes' = decodeAttributes attributes

showBGP x = show x

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
