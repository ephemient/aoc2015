module Main (main) where

import Day1 (day1a, day1b)
import Paths_aoc2015 (getDataFileName)

main :: IO ()
main = do
    putStrLn "Day 1"
    day1 <- getDataFileName "day1.txt" >>= readFile
    print $ day1a day1
    print $ day1b day1
