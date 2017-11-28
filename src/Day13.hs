{-# LANGUAGE TupleSections, TypeApplications #-}
module Day13 (day13a, day13b) where

import Data.Functor (($>))
import Data.Function (on)
import Data.Graph.Inductive (Graph, Gr, LEdge, delNode, edgeLabel, insEdges, insNode, lneighbors, mkGraph, newNodes, nodes, toLEdge)
import qualified Data.Map.Lazy as Map (assocs, fromListWith)
import Data.List (elemIndex, nub)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Text.Parsec (ParseError, ParsecT, choice, eof, many1, parse, sepEndBy)
import Text.Parsec.Char (char, digit, letter, newline, string)

points :: (Graph gr, Monad m, Num a, Read a) => ParsecT String u m (gr String a)
points = fmap toGraph . (<* eof) . flip sepEndBy newline $ do
    a <- many1 letter
    string " would "
    op <- choice [string "gain " $> id, string "lose " $> negate]
    n <- op . read <$> many1 digit
    string " happiness units by sitting next to "
    b <- many1 letter
    char '.'
    return (a, b, n)
  where
    toGraph input = mkGraph (zip [0..] nodes) edges where
        nodes = nub $ concat [[a, b] | (a, b, _) <- input]
        edges = map (uncurry toLEdge) . Map.assocs $ Map.fromListWith (+)
          [ ((min x y, max x y), c)
          | (a, b, c) <- input
          , let Just x = elemIndex a nodes
          , let Just y = elemIndex b nodes
          ]

circuits :: (Graph gr) => gr a b -> [[LEdge b]]
circuits g = finish . map fst . last . takeWhile (not . null) $ iterate grow
    [([(n, start, l)], delNode start g) | (l, n) <- lneighbors g start]
  where
    start:_ = nodes g
    grow = concatMap $ \(path@((m, _, _):_), g') ->
        [((n, m, l):path, delNode m g') | (l, n) <- lneighbors g' m]
    finish = mapMaybe $ \path@((n, _, _):_) ->
        fmap ((:path) . (start, n,)) . lookup n . map swap $ lneighbors g start

day13a :: String -> Either ParseError Int
day13a = fmap (maximum . map (sum . map edgeLabel) . circuits @Gr) .
         parse points ""

day13b :: String -> Either ParseError Int
day13b = fmap (maximum . map (sum . map edgeLabel) . circuits @Gr . addMe) .
         parse points ""
  where
    addMe g = let [n] = newNodes 1 g in
              insEdges [(m, n, 0) | m <- nodes g] $ insNode (n, "") g
