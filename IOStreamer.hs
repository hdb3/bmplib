{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString

import BMPMessage

main = do
    stream <- parserToInputStream bmpParser stdin
    loop stream where
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do print bmpMsg
                              -- putStrLn (identify $ decodeBgp $ L.fromStrict rawMsg)
                              loop stream )
              msg
