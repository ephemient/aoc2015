module Day17 (day17a, day17b) where

import Control.Monad (filterM)

fill :: (Num a, Ord a) => ([a] -> Bool) -> a -> [a] -> [[a]]
fill f n = filter ((== n) . sum) . filter f . filterM (const [False, True])

day17a :: Int -> String -> Int
day17a n = length . fill (const True) n . map read . lines

day17b :: Int -> String -> Int
day17b n string =
    let containers = map read $ lines string
        count = minimum . map length $ fill (const True) n containers
     in length $ fill ((== count) . length) n containers
