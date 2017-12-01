module Day20 (day20a, day20b) where

import Data.List (find)
import Math.NumberTheory.Primes.Factorisation (factorise)

day20a :: String -> Maybe Integer
day20a s = find ((>= read s `quot` 10) . sumFactors) [1..] where
    sumFactors n =
        product [(p ^ (e + 1) - 1) `div` (p - 1) | (p, e) <- factorise n]

day20b :: String -> Maybe Integer
day20b s = find ((>= read s `quot` 11) . sumFactors') [1..] where
    sumFactors' n = sum [q | (q, 0) <- (n `quotRem`) <$> [1..50]]
