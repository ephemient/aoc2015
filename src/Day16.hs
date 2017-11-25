module Day16 (day16a, day16b) where

import Control.Monad.Except ()
import Data.Map (Map)
import qualified Data.Map as Map (assocs, fromList, lookup)
import Text.Parsec (ParseError, ParsecT, eof, many1, parse, sepBy, sepEndBy)
import Text.Parsec.Char (alphaNum, newline, string)

aunts :: (Monad m, Read a) => ParsecT String u m [(String, Map String a)]
aunts = (<* eof) . flip sepEndBy newline $ do
    string "Sue "
    ident <- many1 alphaNum
    string ": "
    attrs <- fmap Map.fromList . flip sepBy (string ", ") $ do
        compound <- many1 alphaNum
        string ": "
        amount <- read <$> many1 alphaNum
        return (compound, amount)
    return (ident, attrs)

sue :: Map String Int
sue = Map.fromList
        [ ("children", 3)
        , ("cats", 7)
        , ("samoyeds", 2)
        , ("pomeranians", 3)
        , ("akitas", 0)
        , ("vizslas", 0)
        , ("goldfish", 5)
        , ("trees", 3)
        , ("cars", 2)
        , ("perfumes", 1)
        ]

day16a :: String -> Either ParseError String
day16a = fmap (unwords . map fst . filter (isSue . snd)) . parse aunts "" where
    isSue attrs = all matches $ Map.assocs sue where
        matches (compound, amount) =
            maybe True (== amount) $ Map.lookup compound attrs

day16b :: String -> Either ParseError String
day16b = fmap (unwords . map fst . filter (isSue . snd)) . parse aunts "" where
    isSue attrs = all matches $ Map.assocs sue where
        matches (compound, amount) =
            maybe True (check compound amount) $ Map.lookup compound attrs
        check compound x y
          | compound `elem` ["cats", "trees"] = x < y
          | compound `elem` ["pomeranians", "goldfish"] = x > y
          | otherwise = x == y
