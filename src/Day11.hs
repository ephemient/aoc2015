module Day11 (day11a, day11b, nextOk) where

import Data.List (group, nub, tails)

next :: String -> String
next (c:rest)
  | c /= 'z', all (== 'z') rest = skip c : map (const 'a') rest
  | otherwise = c : next rest
  where
    skip c
      | succ c `elem` "iol" = succ $ succ c
      | otherwise = succ c

ok :: String -> Bool
ok string =
    not (any (`elem` "iol") string) &&
    any increasing (tails string) &&
    not (null . drop 1 . nub $ pairs string)
  where
    increasing (a:b:c:_) | succ a == b && succ b == c = True
    increasing _ = False
    pairs = map head . filter (not . null . drop 1) . group

nextOk :: String -> String
nextOk = head . filter ok . iterate next . next

day11a :: String -> String
day11a = nextOk

day11b :: String -> String
day11b = nextOk . nextOk
