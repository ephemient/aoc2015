module Day24 (day24a, day24b) where

import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (last)

grab :: (Num a, Ord a) => Int -> a -> [a] -> [([a], [a])]
grab = grab' [] [] where
    grab' yes no _ 0 xs = [(yes, no ++ xs)]
    grab' _ _ 0 _ _ = []
    grab' yes no n s (x:xs)
      | x <= s = grab' (x:yes) no (n - 1) (s - x) xs ++ grab' yes (x:no) n s xs
      | otherwise = grab' yes (x:no) n s xs
    grab' _ _ _ _ _ = []

day24a :: String -> Maybe Int
day24a input = minimum . map (product . fst) . NonEmpty.last <$> do
    nonEmpty . takeWhile (not . null) .  iterate f . filter check $
        grab maxBound target items
  where
    items = read <$> lines input
    (target, 0) = sum items `divMod` 3
    check (_, xs) = not . null $ grab maxBound target xs
    f ((xs, _):_) = grab (length xs - 1) target items

day24b :: String -> Maybe Int
day24b input = minimum . map (product . fst) . NonEmpty.last <$> do
    nonEmpty . takeWhile (not . null) .  iterate f . filter check $
        grab maxBound target items
  where
    items = read <$> lines input
    (target, 0) = sum items `divMod` 4
    check (_, xs) = any check' $ grab maxBound target xs
    check' (_, xs) = not . null $ grab maxBound target xs
    f ((xs, _):_) = grab (length xs - 1) target items
