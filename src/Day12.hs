{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Day12 (day12a, day12b) where

import Data.Aeson (Value(Array, Number, Object, String), decode)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.List (foldl')
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Text (pack)

jsum :: (Bounded a, Integral a) => a -> Value -> a
jsum k (Object o) = foldl' jsum k o
jsum k (Array a) = foldl' jsum k a
jsum !k (Number (toBoundedInteger -> Just n)) = k + n
jsum k _ = k

jsum' :: (Bounded a, Integral a) => a -> Value -> a
jsum' k (Object o) | String (pack "red") `notElem` o = foldl' jsum' k o
jsum' k (Array a) = foldl' jsum' k a
jsum' !k (Number (toBoundedInteger -> Just n)) = k + n
jsum' k _ = k

day12a :: String -> Maybe Int
day12a = fmap (jsum 0) . decode . BS.pack

day12b :: String -> Maybe Int
day12b = fmap (jsum' 0) . decode . BS.pack
