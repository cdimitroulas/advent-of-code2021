{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AOC.Challenge.Day8 where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           GHC.Unicode          (isAlpha, isSpace)

type SignalPattern = Text

type OutputValue = (Text, Text, Text, Text)

isUniqueSegment :: Text -> Bool
isUniqueSegment x = case T.length x of
  2 -> True
  3 -> True
  4 -> True
  7 -> True
  _ -> False

day8Part1 :: [([SignalPattern], OutputValue)] -> Int
day8Part1 = length . concatMap (getUniqueVals . snd)
  where
    getUniqueVals (val1, val2, val3, val4) =
      filter isUniqueSegment [val1, val2, val3, val4]

day8Part2 :: [([SignalPattern], OutputValue)] -> Int
day8Part2 = error "not implemented. Oh god this is confusing"

main :: IO ()
main = do
  input <- P.parseOnly inputParser <$> TIO.readFile "data/day8.txt"
  putStrLn "Day 8 part one:"
  print (day8Part1 <$> input)

  putStrLn "Day 8 part two:"
  print (day8Part2 <$> input)

outputValueParser :: Parser OutputValue
outputValueParser = do
  P.skip isSpace
  val1 <- P.takeWhile isAlpha
  P.skip isSpace
  val2 <- P.takeWhile isAlpha
  P.skip isSpace
  val3 <- P.takeWhile isAlpha
  P.skip isSpace
  val4 <- P.takeWhile isAlpha
  return (val1, val2, val3, val4)

signalPatternsParser :: Parser [SignalPattern]
signalPatternsParser = P.takeWhile isAlpha `P.sepBy` P.char ' '

lineParser :: Parser ([SignalPattern], OutputValue)
lineParser = do
  signalPatternsText <- P.takeWhile (/= '|')
  P.skip (== '|')
  outputValue <- outputValueParser
  return ((filterEmptyStrings . T.splitOn " ") signalPatternsText, outputValue)

inputParser :: Parser [([SignalPattern], OutputValue)]
inputParser = P.many' $ lineParser <* P.endOfLine

filterEmptyStrings :: [Text] -> [Text]
filterEmptyStrings = filter (/= T.empty)
