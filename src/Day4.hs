module Day4 (day4a, day4b) where

import Control.Monad (guard)
import Crypto.Hash (Digest, MD5, hash)
import Data.Bits ((.&.))
import Data.ByteArray (Bytes, convert)
import Data.ByteArray.Parse (Result(ParseOK), anyByte, byte, parse)
import Data.ByteString.Char8 (pack)
import Data.List (findIndex)

day4a :: String -> Maybe Int
day4a string = findIndex hasZeros
               [hash . pack $ string ++ show n :: Digest MD5 | n <- [0..]] where
    hasZeros bs =
        case parse zeros (convert bs :: Bytes)
          of ParseOK _ _ -> True
             _ -> False
    zeros = do
        byte 0
        byte 0
        word <- anyByte
        guard $ word .&. 0xF0 == 0x00

day4b :: String -> Maybe Int
day4b string = findIndex hasZeros
               [hash . pack $ string ++ show n :: Digest MD5 | n <- [0..]] where
    hasZeros bs =
        case parse zeros (convert bs :: Bytes)
          of ParseOK _ _ -> True
             _ -> False
    zeros = do
        byte 0
        byte 0
        byte 0
