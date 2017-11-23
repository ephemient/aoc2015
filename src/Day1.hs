module Day1 (day1a, day1b) where

import Data.List (findIndex)

sign :: Char -> Int
sign '(' = 1
sign ')' = -1

day1a :: String -> Int
day1a = sum . map sign

day1b :: String -> Maybe Int
day1b = findIndex (< 0) . scanl (+) 0 . map sign
