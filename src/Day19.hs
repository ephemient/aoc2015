module Day19 (day19a, day19b) where

import Data.List (findIndex, foldl', inits, isPrefixOf, nub, tails)
import qualified Data.Map.Strict as Map (empty, foldlWithKey', insertWith, member, null, singleton)
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

search :: (Ord a) => [a] -> [([a], [a])] -> [a] -> Maybe Int
search target replacements start =
    findIndex (Map.member target) . takeWhile (not . Map.null) $
    iterate next (Map.singleton start 0)
  where
    next = Map.foldlWithKey' add Map.empty
    add m s k = foldl' (flip . uncurry $ Map.insertWith min) m $ rstep s k
    rstep s k =
      [ (prefix ++ from ++ dropZip to rest, max 0 $ k - length to)
      | (from, to) <- replacements
      , (prefix, rest) <- drop (k - length to + 1) $ zip (inits s) (tails s)
      , to `isPrefixOf` rest
      ]

day19a :: String -> Either ParseError Int
day19a string = do
    (start, replacements) <- parse machine "" string
    return . length . nub $ step replacements start

day19b :: String -> Either ParseError (Maybe Int)
day19b string = do
    (target, replacements) <- parse machine "" string
    return $ search "e" replacements target
