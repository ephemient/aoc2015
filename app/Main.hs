module Main (main) where

import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Day5 (day5a, day5b)
import Day6 (day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)
import Day10 (day10a, day10b)
import Day11 (day11a, day11b)
import Day12 (day12a, day12b)
import Day13 (day13a, day13b)
import Day14 (day14a, day14b)
import Day15 (day15a, day15b)
import Day16 (day16a, day16b)
import Day17 (day17a, day17b)
import Paths_aoc2015 (getDataFileName)

run :: Int -> [String -> String] -> IO ()
run i funcs = do
    putStrLn $ "Day " ++ show i
    contents <- getDataFileName ("day" ++ show i ++ ".txt") >>= readFile
    mapM_ (putStrLn . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 [show . day1a, maybe "(\x22a5)" show . day1b]
    run 2 [either show show . day2a, either show show . day2b]
    run 3 [show . day3a, show . day3b]
    run 4 [maybe "(\x22a5)" show . day4a, maybe "(\x22a5)" show . day4b]
    run 5 [show . day5a, show . day5b]
    run 6 [either show show . day6a, either show show . day6b]
    run 7 [either show show . day7a, either show show . day7b]
    run 8 [show . day8a, show . day8b]
    run 9 [maybe "(\x22a5)" show . day9a, maybe "(\x22a5)" show . day9b]
    run 10 [show . day10a, show . day10b]
    run 11 [day11a, day11b]
    run 12 [maybe "(\x22a5)" show . day12a, maybe "(\x22a5)" show . day12b]
    run 13 [either show show . day13a, either show show . day13b]
    run 14 [either show show . day14a, either show show . day14b]
    run 15 [either show show . day15a, either show show . day15b]
    run 16 [either show id . day16a, either show id . day16b]
    run 17 [show . day17a 150, show . day17b 150]
