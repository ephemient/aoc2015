module Day14 (day14a, day14b) where

import Data.List (group, sort)
import Text.Parsec (ParseError, ParsecT, eof, many1, parse, sepEndBy)
import Text.Parsec.Char (digit, letter, newline, string)

reindeer :: (Monad m, Read a) => ParsecT String u m [(String, a, a, a)]
reindeer = (<* eof) . flip sepEndBy newline $ do
    name <- many1 letter
    string " can fly "
    speed <- read <$> many1 digit
    string " km/s for "
    time <- read <$> many1 digit
    string " seconds, but then must rest for "
    wait <- read <$> many1 digit
    string " seconds."
    return (name, speed, time, wait)

flight :: (Integral a) => a -> (s, a, a, a) -> a
flight duration (_, speed, time, wait) =
    let (periods, extra) = duration `quotRem` (time + wait)
     in speed * (time * periods + min extra time)

winners :: (Integral a) => [(s, a, a, a)] -> a -> [s]
winners reindeers duration =
    let flights = map (flight duration) reindeers
        best = maximum flights
     in [ s
        | ((s, _, _, _), distance) <- zip reindeers flights
        , distance == best
        ]

day14a :: String -> Either ParseError Int
day14a = fmap (maximum . map (flight 2503)) . parse reindeer ""

day14b :: String -> Either ParseError Int
day14b = fmap (accumWinners . scores [1..2503]) . parse reindeer "" where
    scores durations reindeers = concatMap (winners reindeers) durations
    accumWinners = maximum . map length . group . sort
