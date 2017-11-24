module Day6 (day6a, day6b) where

import Control.Monad.Except ()
import Data.Array ((!), (//), elems, listArray)
import Data.Ix (range)
import Text.Parsec (ParseError, choice, eof, getState, many1, modifyState, runParser, sepEndBy, try)
import Text.Parsec.Char (char, digit, newline, string)

total :: (Num a) => [(String, a -> a)] -> String -> Either ParseError a
total ops = fmap (sum . elems) .
            runParser rules (listArray ((0, 0), (999, 999)) $ repeat 0) "" where
    rules = sepEndBy rule newline >> eof >> getState
    rule = do
        op <- choice [try $ string a >> return b | (a, b) <- ops]
        char ' '
        first <- xy
        string " through "
        second <- xy
        modifyState $ \a -> a // [(i, op $ a ! i) | i <- range (first, second)]
    xy = do
        x <- many1 digit
        char ','
        y <- many1 digit
        return (read x, read y)

day6a :: String -> Either ParseError Int
day6a = total [("turn on", const 1), ("turn off", const 0), ("toggle", (1 -))]

day6b :: String -> Either ParseError Int
day6b = total
  [ ("turn on", (+ 1))
  , ("turn off", max 0 . subtract 1)
  , ("toggle", (+ 2))
  ]
