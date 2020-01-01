{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, ViewPatterns #-}
module Day23 (day23a, day23b) where

import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map (adjust, empty, findWithDefault, insertWith, singleton)
import Text.Parsec (ParseError, ParsecT, (<|>), choice, eof, many1, parse, sepEndBy, try)
import Text.Parsec.Char (alphaNum, char, digit, newline, string)
import Text.Read (readMaybe)

data Instruction r a = HLF r | TPL r | INC r | JMP a | JIE r a | JIO r a
  deriving Show

machine :: (Monad m, Integral a) => ParsecT String u m [Instruction String a]
machine = (instruction `sepEndBy` newline) <* eof where
    instruction = choice
      [ HLF <$> (try (string "hlf ") *> many1 alphaNum)
      , TPL <$> (try (string "tpl ") *> many1 alphaNum)
      , INC <$> (try (string "inc ") *> many1 alphaNum)
      , JMP <$> (try (string "jmp ") *> num)
      , JIE <$> (try (string "jie ") *> many1 alphaNum) <*> (string ", " *> num)
      , JIO <$> (try (string "jio ") *> many1 alphaNum) <*> (string ", " *> num)
      ]
    num = ((char '+' $> id) <|> (char '-' $> negate)) <*> digits
    digits = foldl' f 0 . map digitToInt <$> many1 digit
    f x y = 10 * x + fromIntegral y

run :: (Ord k, Integral a) => [Instruction k a] -> Map k a -> Map k a
run instructions regs = run' [] instructions regs where
    run' prev isns@(cur:next) regs = case cur of
        HLF reg -> run' (cur:prev) next $ Map.adjust (`div` 2) reg regs
        TPL reg -> run' (cur:prev) next $ Map.adjust (* 3) reg regs
        INC reg -> run' (cur:prev) next $ Map.insertWith (+) reg 1 regs
        JMP off -> rot (fromIntegral off) prev isns regs
        JIE reg off | even $ Map.findWithDefault 0 reg regs ->
            rot (fromIntegral off) prev isns regs
        JIO reg off | Map.findWithDefault 0 reg regs == 1 ->
            rot (fromIntegral off) prev isns regs
        _ -> run' (cur:prev) next regs
    run' _ _ regs = regs
    rot n prev isns regs | n < 0 = run' prev' (reverse isns' ++ isns) regs
      where (isns', prev') = splitAt (-n) prev
    rot n prev isns regs = run' (reverse prev' ++ prev) isns' regs
      where (prev', isns') = splitAt n isns

day23a :: String -> Either ParseError Int
day23a input = do
    instructions <- parse machine "" input
    return . Map.findWithDefault 0 "b" $ run instructions Map.empty

day23b :: String -> Either ParseError Int
day23b input = do
    instructions <- parse machine "" input
    return . Map.findWithDefault 0 "b" . run instructions $ Map.singleton "a" 1
