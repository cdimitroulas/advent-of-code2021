{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module AOC.Challenge.Day6 where

import           Data.Coerce     (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           GHC.OldList     (genericLength)
import           Text.Read       (readMaybe)

-- A lanternfish is represented by a number that represent the number of days until it
-- creates a new lanternfish
newtype Fish = Fish {unFish :: Int} deriving (Show)

-- A new fish is born with 8 days until it produces a new fish
newFish :: Fish
newFish = Fish 8

isGivingBirth :: Fish -> Bool
isGivingBirth = (== 0) . unFish

-- What happens to the fish after a day passes.
-- A fish with zero days left resets to 6 days.
nextFish :: Fish -> [Fish]
nextFish fish
  | isGivingBirth fish = [Fish 6, newFish]
  | otherwise = [Fish (coerce fish - 1)]

-- This solution is very inefficient. It doesn't work for day 2 in a reasonable amount of time.
day6part1 :: [Fish] -> Integer
day6part1 = run 0
  where
    run :: Int -> [Fish] -> Integer
    run day fishList
      | day == 80 = genericLength fishList
      | otherwise = run (day + 1) newFishes
      where
        newFishes = concatMap nextFish fishList

-- Optimization ideas:
-- - For new fish, only track their date of birth. It should be easy to calculate how many times
--   these new fish will give birth in a given date range (final day - day of birth) / 8
--
--  - ^ that didn't work... try a Map data structure instead of arrays.

-- A new fish generation is represented by the day of birth and number of new fish.
type Generation = Map DaysTillNextBirth Count

type DaysTillNextBirth = Int

type Count = Integer

makeGeneration :: [Fish] -> Generation
makeGeneration = foldr (\fish -> M.insertWith (+) (unFish fish) 1) M.empty

nextGeneration :: Generation -> Generation
nextGeneration gen =
  ( M.delete (-1)
      . M.insertWith (+) 6 births
      . M.insertWith (+) 8 births
  )
    newGen
  where
    newGen = M.mapKeys (subtract 1) gen
    -- the number of births is equal to the number of fish which are at the -1 key
    births = fromMaybe 0 $ M.lookup (-1) newGen

populationAfterDays :: Int -> [Fish] -> Integer
populationAfterDays n = sum . (!! n) . iterate nextGeneration . makeGeneration

-- more efficient solution for part 1
day6part1' :: [Fish] -> Integer
day6part1' = populationAfterDays 80

day6part2 :: [Fish] -> Integer
day6part2 = populationAfterDays 256

parseInput :: Text -> Maybe [Fish]
parseInput =
  mapM
    ((Fish `fmap`) . readMaybe . T.unpack)
    . concatMap (T.splitOn ",")
    . T.lines

main :: IO ()
main = do
  input <- parseInput <$> TIO.readFile "data/day6.txt"
  putStrLn "Day 6 part 1:"
  print $ day6part1' <$> input

  putStrLn "Day 6 part 2:"
  print $ day6part2 <$> input
