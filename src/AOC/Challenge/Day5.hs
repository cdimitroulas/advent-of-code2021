{-# LANGUAGE TupleSections #-}

module AOC.Challenge.Day5 where

import           AOC.Point            (Line (..), Point (..), isHorizontalLine,
                                       isVerticalLine, toDiscretePoints)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import           Data.Char            (isDigit)
import           Data.Foldable        (foldl')
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as M
import           Data.Text            (unpack)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Text.Read            (readMaybe)

-- Filter lines to get only vertical/horizontal ones.
-- Use toDiscretePoints to get all the points for all the lines
-- Count all the occurrences of each point
-- Count how many points occurred at least twice
day5part1 :: NonEmpty Line -> Int
day5part1 =
  M.size
    . M.filter (>= 2)
    . foldl' (\m point -> M.insertWith (+) point (1 :: Int) m) M.empty
    . concatMap toDiscretePoints
    . NE.filter (\x -> isHorizontalLine x || isVerticalLine x)

day5part2 :: NonEmpty Line -> Int
day5part2 =
  M.size
    . M.filter (>= 2)
    . foldl' (\m point -> M.insertWith (+) point (1 :: Int) m) M.empty
    . concatMap toDiscretePoints

parseLine :: Parser Line
parseLine = do
  x1 <- read . unpack <$> Parser.takeWhile isDigit
  Parser.skipMany1 (Parser.char ',')
  y1 <- read. unpack <$> Parser.takeWhile isDigit
  Parser.skipWhile (not . isDigit)
  x2 <- read. unpack <$> Parser.takeWhile isDigit
  Parser.skipMany1 (Parser.char ',')
  y2 <- read . unpack <$> Parser.takeWhile isDigit
  return (Line (Point x1 y1) (Point x2 y2))

parseInt :: Char -> Maybe Int
parseInt c = readMaybe [c]

main :: IO ()
main = do
  ls <- mapM (Parser.parseOnly parseLine) . T.lines <$> TIO.readFile "data/day5.txt"
  putStrLn "Day 5 part one:"
  print (day5part1 . NE.fromList <$> ls)

  putStrLn "Day 5 part two:"
  print (day5part2 . NE.fromList <$> ls)
