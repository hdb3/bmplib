{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString
import Data.Attoparsec.ByteString -- from package attoparsec

import BMPMessage

main = go rawBMPMessageParser action2
-- main = go bmpParser print

action2 msg = do
    print msg
    let msg' = extract msg
    parseTest bmpMessageParser' msg'

go parser action = do

    stream <- parserToInputStream parser stdin
    loop stream where
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do action bmpMsg
                              loop stream )
              msg
