{-# LANGUAGE BangPatterns #-}
module Day8 (day8a, day8b) where

import Data.List (foldl')

scanEscapes :: Int -> String -> Int
scanEscapes !k ('"' : s) = scanEscapes (k + 1) s
scanEscapes !k ('\\' : '\\' : s) = scanEscapes (k + 1) s
scanEscapes !k ('\\' : '"' : s) = scanEscapes (k + 1) s
scanEscapes !k ('\\' : 'x' : s) = scanEscapes (k + 3) (drop 2 s)
scanEscapes k (_:s) = scanEscapes k s
scanEscapes k _ = k

scanUnescape :: Int -> String -> Int
scanUnescape !k ('"' : s) = scanUnescape (k + 1) s
scanUnescape !k ('\\' : s) = scanUnescape (k + 1) s
scanUnescape k (_ : s) = scanUnescape k s
scanUnescape !k _ = k + 2

day8a :: String -> Int
day8a = foldl' scanEscapes 0 . lines

day8b :: String -> Int
day8b = foldl' scanUnescape 0 . lines
