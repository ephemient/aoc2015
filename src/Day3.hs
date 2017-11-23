module Day3 (day3a, day3b) where

import Data.List (nub)

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y + 1)
move (x, y) 'v' = (x, y - 1)
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)

day3a :: String -> Int
day3a = length . nub . scanl move (0, 0)

day3b :: String -> Int
day3b moves = length . nub $ santa ++ robosanta where
    santa = scanl move (0, 0) . map fst . filter snd . zip moves $ cycle [True, False]
    robosanta = scanl move (0, 0) . map fst . filter snd . zip moves $ cycle [False, True]
