{-# LANGUAGE TypeApplications #-}
module Day4 (day4a, day4b) where

import Control.Monad (guard)
import Control.Parallel.Strategies (parBuffer, rseq, withStrategy)
import Crypto.Hash (Digest, HashAlgorithm, MD5, hash)
import Data.Bits ((.&.))
import Data.ByteArray (Bytes, convert)
import Data.ByteArray.Parse (Result(ParseOK), anyByte, byte, parse)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (findIndex)
import GHC.Conc (numCapabilities)

hashes :: (HashAlgorithm a) => String -> [Digest a]
hashes string = withStrategy (parBuffer numCapabilities rseq) $
    hash . pack . (string ++) . show <$> [0 :: Int ..]

day4a :: String -> Maybe Int
day4a = findIndex hasZeros . hashes @MD5 where
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
day4b = findIndex hasZeros . hashes @MD5 where
    hasZeros bs =
        case parse zeros (convert bs :: Bytes)
          of ParseOK _ _ -> True
             _ -> False
    zeros = do
        byte 0
        byte 0
        byte 0
