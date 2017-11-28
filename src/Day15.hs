module Day15 (day15a, day15b, optimalCookie) where

import Control.Monad (ap)
import Data.Functor (($>))
import Data.List (maximumBy, transpose)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.Parsec (ParseError, eof, many1, option, parse, sepBy, sepEndBy)
import Text.Parsec.Char (char, digit, letter, newline, string)

optimalCookie :: (Enum a, Num a, Ord a, Read a, Ord b) => ([a] -> Bool) -> ([a] -> b) -> [String] -> String -> Either ParseError ([a], [a])
optimalCookie pred cmp categories = fmap optimize . parse ingredients "" where
    ingredients = fmap (map extract) . (<* eof) . flip sepEndBy newline $ do
        many1 letter
        string ": "
        flip sepBy (string ", ") $ do
            category <- many1 letter
            char ' '
            value <- option id (char '-' $> negate) `ap` (read <$> many1 digit)
            return (category, value)
    extract props = map (fromMaybe 0 . flip lookup props) categories
    choose n k
      | n <= 1 = [[k]]
      | otherwise = [j:l | j <- [0..k], l <- choose (n - 1) (k - j)]
    optimize values = maximumBy (comparing $ cmp . snd)
      [ (counts, totals)
      | counts <- choose (length values) 100
      , let totals = map (max 0 . sum) . transpose $ zipWith (map . (*)) counts values
      , pred totals
      ]

day15a :: String -> Either ParseError Int
day15a = fmap (product . snd) . optimalCookie (const True) product
         ["capacity", "durability", "flavor", "texture"]

day15b :: String -> Either ParseError Int
day15b = fmap (product . tail . snd) . optimalCookie ((== 500) . head) (product . tail)
         ["calories", "capacity", "durability", "flavor", "texture"]
