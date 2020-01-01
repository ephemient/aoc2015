module Day21 (day21a, day21b) where

import Control.Arrow ((***))
import Data.List (find, sortBy)
import Data.Ord (Down(..), comparing)
import Text.Parsec (ParseError, ParsecT, eof, many1, optional, parse)
import Text.Parsec.Char (digit, newline, string)
import Text.Read (readMaybe)

data Stats a = Stats { hp :: a, atk :: a, def :: a }

pick :: Int -> Int -> [a] -> [[a]]
pick lo hi | 0 <= lo, lo <= hi = pick' lo hi where
    pick' _ 0 _ = [[]]
    pick' 0 hi' xs = [] : pick' 1 hi' xs
    pick' lo' hi' xs@(x:xs') =
        map (x:) (pick (max 0 $ lo' - 1) (hi' - 1) xs') ++ pick lo' hi' xs'
    pick' _ _ _ = []
pick _ _ = const []

rpg :: (Monad m, Read a) => ParsecT String u m (Stats a)
rpg = do
    Just hp <- fmap readMaybe $ string "Hit Points: " *> many1 digit <* newline
    Just atk <- fmap readMaybe $ string "Damage: " *> many1 digit <* newline
    Just def <- fmap readMaybe $
        string "Armor: " *> many1 digit <* optional newline <* eof
    return $ Stats hp atk def

weapons, armors, rings :: (Num a) => [(a, Stats a -> Stats a)]
weapons =
  [ (8, \stats -> stats {atk = atk stats + 4})
  , (10, \stats -> stats {atk = atk stats + 5})
  , (25, \stats -> stats {atk = atk stats + 6})
  , (40, \stats -> stats {atk = atk stats + 7})
  , (74, \stats -> stats {atk = atk stats + 8})
  ]
armors =
  [ (13, \stats -> stats {def = def stats + 1})
  , (31, \stats -> stats {def = def stats + 2})
  , (53, \stats -> stats {def = def stats + 3})
  , (75, \stats -> stats {def = def stats + 4})
  , (102, \stats -> stats {def = def stats + 5})
  ]
rings =
  [ (25, \stats -> stats {atk = atk stats + 1})
  , (50, \stats -> stats {atk = atk stats + 2})
  , (100, \stats -> stats {atk = atk stats + 3})
  , (20, \stats -> stats {def = def stats + 1})
  , (40, \stats -> stats {def = def stats + 2})
  , (80, \stats -> stats {def = def stats + 3})
  ]

beats :: (Num a, Ord a) => Stats a -> Stats a -> Bool
beats player boss = beats' True player boss where
     beats' ok (Stats hp _ _) _ | hp <= 0 = not ok
     beats' ok p@(Stats _ atk _) q@(Stats hp _ def) =
        ok `seq` beats' (not ok) q {hp = hp - max 1 (atk - def)} p

day21a :: String -> Either ParseError (Maybe Int)
day21a string = do
    boss <- parse rpg "" string
    return . fmap fst . find ((`beats` boss) . snd) . sortBy (comparing fst) $ do
        a <- pick 1 1 weapons
        b <- pick 0 1 armors
        c <- pick 0 2 rings
        return . (sum *** foldr ($) (Stats 100 0 0)) . unzip $ a ++ b ++ c

day21b :: String -> Either ParseError (Maybe Int)
day21b string = do
    boss <- parse rpg "" string
    return . fmap fst . find (not . (`beats` boss) . snd) .
        sortBy (comparing $ Down . fst) $ do
        a <- pick 1 1 weapons
        b <- pick 0 1 armors
        c <- pick 0 2 rings
        return . (sum *** foldr ($) (Stats 100 0 0)) . unzip $ a ++ b ++ c
