{-# LANGUAGE DataKinds #-}
module Day25 (day25) where

import Data.Char (isDigit)
import Data.Finite (Finite, getFinite)

day25 :: String -> Int
day25 input =
    fromIntegral $ getFinite (20151125 * 252533 ^ n :: Finite 33554393) where
    [x, y] = map read . filter (not . null) . map (filter isDigit) $ words input
    n = ((x + y - 1) * (x + y - 2)) `div` 2 + y - 1 :: Int
