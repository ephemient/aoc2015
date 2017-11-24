module Day5 (day5a, day5b) where

import Data.List (isInfixOf)

day5a :: String -> Int
day5a = length . filter nice . lines where
    nice string = not $
        null (drop 2 $ filter (`elem` "aeiou") string) ||
        not (any (uncurry (==)) (zip string (tail string))) ||
        any (`isInfixOf` string) ["ab", "cd", "pq", "xy"]

day5b :: String -> Int
day5b = length . filter nice . lines where
    nice string = xyxy string && aba string
    xyxy (x:y:rest) | [x, y] `isInfixOf` rest = True
    xyxy (_:rest) = xyxy rest
    xyxy _ = False
    aba (x:y:z:_) | x == z = True
    aba (_:rest) = aba rest
    aba _ = False
