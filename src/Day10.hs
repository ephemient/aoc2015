module Day10 (day10a, day10b, ls) where

import Data.List (group)

ls :: String -> String
ls = concatMap f . group where f s = show (length s) ++ take 1 s

day10a :: String -> Int
day10a = length . (!! 40) . iterate ls

day10b :: String -> Int
day10b = length . (!! 50) . iterate ls
