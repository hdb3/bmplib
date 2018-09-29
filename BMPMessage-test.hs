{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16
import Data.Attoparsec.ByteString -- from package attoparsec

import BMPMessage
fromHex = fst . Data.ByteString.Base16.decode

s1 = fromHex "04000100964a756e69706572204e6574776f726b732c20496e632e206d33323020696e7465726e657420726f757465722c206b65726e656c204a554e4f532031352e3152342e362c204275696c6420646174653a20323031362d30362d32332032323a30353a30362055544320436f707972696768742028632920313939362d32303136204a756e69706572204e6574776f726b732c20496e632e000200086d3332302d726530"

main'' = parseTest simpleParser ( s1 `BS.append` BS.replicate 1000 0)
main' = parseTest bmpMessageParser' ( s1 `BS.append` BS.replicate 1000 0)

parse' p bs = feed (parse p bs) BS.empty

main = print $ parse' bmpMessageParser' s1

simpleParser :: Parser Int
simpleParser = do
    bs <- takeByteString
    return (BS.length bs)
