{-# LANGUAGE TypeApplications, ViewPatterns #-}
module Day9 (day9a, day9b) where

import Data.Graph.Inductive (Graph, Gr, LEdge, delNode, edgeLabel, isConnected, lneighbors, mkGraph, nodes)
import Data.List (elemIndex, nub)

parse :: (Read a) => String -> Maybe (String, String, a)
parse line =
    case words line
      of [a, "to", b, "=", reads -> (c, ""):_] -> Just (a, b, c)
         _ -> Nothing

toGraph :: (Graph gr, Eq a) => [(a, a, b)] -> gr a b
toGraph input = mkGraph (zip [0..] nodes) edges where
    nodes = nub $ concat [[a, b] | (a, b, _) <- input]
    edges =
      [ (i, j, c)
      | (a, b, c) <- input
      , let Just i = elemIndex a nodes
      , let Just j = elemIndex b nodes
      ]

eulerPaths :: (Graph gr) => gr a b -> [[LEdge b]]
eulerPaths gr | isConnected gr =
    map fst . last . takeWhile (not . null) $ iterate next
      [ ([(b, a, c)], delNode a gr)
      | a <- nodes gr
      , (c, b) <- lneighbors gr a
      ]
  where
    next = concatMap $ \(path@((a, _, _):_), gr') ->
      [ ((b, a, c):path, delNode a gr')
      | (c, b) <- lneighbors gr' a
      ]

minPath, maxPath :: (Real a) => [[LEdge a]] -> a
minPath = minimum . map (sum . map edgeLabel)
maxPath = maximum . map (sum . map edgeLabel)

day9a :: String -> Maybe Int
day9a = fmap (minPath . eulerPaths . toGraph @Gr) . mapM parse . lines

day9b :: String -> Maybe Int
day9b = fmap (maxPath . eulerPaths . toGraph @Gr) . mapM parse . lines
