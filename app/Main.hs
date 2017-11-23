{-# LANGUAGE ExistentialQuantification #-}
module Main (main) where

import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Paths_aoc2015 (getDataFileName)

run :: Int -> [String -> String] -> IO ()
run i funcs = do
    putStrLn $ "Day " ++ show i
    contents <- getDataFileName ("day" ++ show i ++ ".txt") >>= readFile
    mapM_ (putStrLn . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 [show . day1a, show . day1b]
    run 2 [show . day2a, show . day2b]
