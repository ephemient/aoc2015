module Day2 (day2a, day2b) where

import Control.Monad.Except ()
import Text.Parsec (ParseError, Parsec, eof, many1, parse)
import Text.Parsec.Char (char, digit)

dimensions :: Parsec String () (Int, Int, Int)
dimensions =  do
    x <- many1 digit; char 'x'
    y <- many1 digit; char 'x'
    z <- many1 digit; eof
    return (read x, read y, read z)

paper :: (Int, Int, Int) -> Int
paper (x, y, z) = 2 * sum sides + minimum sides where
    sides = [x * y, y * z, x * z]

ribbon :: (Int, Int, Int) -> Int
ribbon (x, y, z) = 2 * minimum [x + y, y + z, x + z] + x * y * z

day2a :: String -> Either ParseError Int
day2a = fmap sum . mapM (fmap paper . parse dimensions "") . lines

day2b :: String -> Either ParseError Int
day2b = fmap sum . mapM (fmap ribbon . parse dimensions "") . lines
