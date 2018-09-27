{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString

import BMPMessage

main = do
    -- is <- handleToInputStream stdin
    stream <- parserToInputStream rawBMPMessageParser stdin
    loop stream where
    -- decodeBgp = decode :: L.ByteString -> BGPMessage
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \BMPMessageRaw{..} -> do putStrLn $ show msgType ++ " : " ++ show (hbLength payload)
                                         -- putStrLn (identify $ decodeBgp $ L.fromStrict rawMsg)
                                         loop stream )
              msg
