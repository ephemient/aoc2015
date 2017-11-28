module Day19 (day19a) where

import Data.List (isPrefixOf, inits, nub, tails)
import Text.Parsec (ParseError, ParsecT, eof, many, many1, optional, parse)
import Text.Parsec.Char (alphaNum, newline, string)

machine :: (Monad m) => ParsecT String u m (String, [(String, String)])
machine = do
    replacements <- many $ do
        from <- many1 alphaNum
        string " => "
        to <- many1 alphaNum
        newline
        return (from, to)
    newline
    special <- many1 alphaNum
    optional newline
    eof
    return (special, replacements)

dropZip :: [a] -> [b] -> [b]
dropZip (_:as) (_:bs) = dropZip as bs
dropZip [] bs = bs
dropZip _ [] = []

step :: (Eq a) => [([a], [a])] -> [a] -> [[a]]
step replacements molecule =
  [ prefix ++ to ++ dropZip from rest
  | (prefix, rest) <- zip (inits molecule) (tails molecule)
  , (from, to) <- replacements
  , from `isPrefixOf` rest
  ]

day19a :: String -> Either ParseError Int
day19a string = do
    (start, replacements) <- parse machine "" string
    return . length . nub $ step replacements start
