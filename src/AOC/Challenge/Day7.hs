module AOC.Challenge.Day7 where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Foldable        (foldl')
import           Data.List            (sort)
import qualified Data.Text.IO         as TIO

fuelCost :: Int -> [Int] -> Int
fuelCost targetPosition = foldl' (\total crab -> total + abs (crab - targetPosition)) 0

day7part1 :: [Int] -> Int
day7part1 crabs = fuelCost targetPosition crabs
  where
    targetPosition = ((!! (length crabs `div` 2)) . sort) crabs

seriesSum :: Integral a => a -> a
seriesSum n = n * (n + 1) `div` 2

fuelCost' :: Int -> [Int] -> Int
fuelCost' targetPosition crabs = sum costs
  where
    costs = map (seriesSum . abs . (targetPosition `subtract`)) crabs

day7part2 :: [Int] -> Int
day7part2 crabs = minimum allFuelCosts
  where
    -- we calculate every possible fuel cost for every position...
    -- there is surely a nice trick to optimize this :D
    allFuelCosts = map (`fuelCost'` crabs) [0 .. length crabs]

parseInput :: Parser [Int]
parseInput = P.decimal `P.sepBy` P.char ','

main :: IO ()
main = do
  crabPositions <- P.parseOnly parseInput <$> TIO.readFile "data/day7.txt"
  putStrLn "Day 7 part one:"
  print (day7part1 <$> crabPositions)

  putStrLn "Day 7 part two:"
  print (day7part2 <$> crabPositions)
