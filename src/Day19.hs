module Day19 (day19a, day19b) where

import Control.Monad (mfilter)
import Data.List (inits, isPrefixOf, mapAccumL, nub, tails, unfoldr)
import qualified Data.Map.Strict as Map (fromListWith, maxViewWithKey, splitLookup)
import qualified Data.Set as Set (insert, member, singleton)
import Data.Maybe (catMaybes)
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
    search' (Set.singleton start) [(0, start)] where
    rmap = Map.fromListWith (++) [(dst, [src]) | (src, dst) <- replacements]
    matchPrefix pre =
        maybe [] ((:[]) . (,) pre) exact ++ unfoldr inexact rmap' where
        (rmap', exact, _) = Map.splitLookup pre rmap
        inexact = mfilter ((`isPrefixOf` pre) . fst . fst) . Map.maxViewWithKey
    search' seen ((d, s):rest)
      | s == target = Just d
      | otherwise = search' seen' . (++ rest) $ map ((,) $! d + 1) new where
        (seen', new) =
            fmap concat . mapAccumL f seen . take 15 $ zip (inits s) (tails s)
        f seen'' (pre, post) =
            fmap concat . mapAccumL (g pre post) seen'' $ matchPrefix post
        g pre post seen'' (dst, srcs) =
            fmap catMaybes $ mapAccumL (h pre post dst) seen'' srcs
        h pre post dst seen'' src
          | Set.member t seen'' = (seen'', Nothing)
          | otherwise = (Set.insert t seen'', Just t)
          where t = pre ++ src ++ drop (length dst) post
    search' _ _ = Nothing

day19a :: String -> Either ParseError Int
day19a string = do
    (start, replacements) <- parse machine "" string
    return . length . nub $ step replacements start

day19b :: String -> Either ParseError (Maybe Int)
day19b string = do
    (target, replacements) <- parse machine "" string
    return $ search "e" replacements target
