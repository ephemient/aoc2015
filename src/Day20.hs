module Day20 (day20a) where

import Data.List (find)
import Math.NumberTheory.Primes.Factorisation (factorise)

day20a :: String -> Maybe Integer
day20a s = find ((>= x) . sumFactors) [1..] where
    x = read s `quot` 10
    sumFactors n =
        product [(p ^ (e + 1) - 1) `div` (p - 1) | (p, e) <- factorise n]
