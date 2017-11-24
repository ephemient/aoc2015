module Day7 (day7a, day7b) where

import Control.Applicative ((<$>))
import Control.Monad (ap)
import Control.Monad.Except ()
import Data.Bits (Bits, (.&.), (.|.), complement, shiftL, shiftR)
import Data.Word (Word16)
import qualified Data.Map.Lazy as Map (fromList, insert, map)
import Data.Map.Lazy (Map, (!))
import Text.Parsec (ParseError, ParsecT, (<|>), choice, eof, many1, runParser, sepEndBy, try)
import Text.Parsec.Char (digit, letter, newline, string)

data Wire a b
  = Const (Either a b)
  | And (Either a b) (Either a b)
  | Or (Either a b) (Either a b)
  | LShift (Either a b) (Either a b)
  | RShift (Either a b) (Either a b)
  | Not (Either a b)

gates :: (Monad m, Read a) => ParsecT String u m (Map String (Wire String a))
gates = fmap Map.fromList $ sepEndBy line newline <* eof where
    line = do
        val <- choice rules
        string " -> "
        reg <- word
        return (reg, val)
    rules = map try
      [ pure And `ap` ref `ap` (string " AND " *> ref)
      , pure Or `ap` ref `ap` (string " OR " *> ref)
      , pure LShift `ap` ref `ap` (string " LSHIFT " *> ref)
      , pure RShift `ap` ref `ap` (string " RSHIFT " *> ref)
      , Not <$> (string "NOT " *> ref)
      , Const <$> ref
      ]
    ref = (Left <$> word) <|> (Right . read <$> many1 digit)
    word = many1 letter

run :: (Ord a, Bits b, Integral b) => Map a (Wire a b) -> Map a b
run wires = values where
    values = Map.map eval wires
    ref = either (values !) id
    eval (Const x) = ref x
    eval (And x y) = ref x .&. ref y
    eval (Or x y) = ref x .|. ref y
    eval (LShift x y) = ref x `shiftL` fromIntegral (ref y)
    eval (RShift x y) = ref x `shiftR` fromIntegral (ref y)
    eval (Not x) = complement $ ref x

day7a :: String -> Either ParseError (Map String Word16)
day7a = fmap run . runParser gates () ""

day7b :: String -> Either ParseError (Map String Word16)
day7b = fmap (run . mangle) . runParser gates () "" where
    mangle wires = Map.insert "b" (Const . Right $ run wires ! "a") wires
