module AOC.Challenge.Day1 where

import           AOC.Common

dataFilePath :: String
dataFilePath = "data/day1.txt"

-- Part One
day1part1 :: [Int] -> Int
day1part1 = getDepthIncreasesN 1

-- Part Two
-- This time we need to look at groups of three values instead of individual values like in
-- part 1

day1part2 :: [Int] -> Int
day1part2 = getDepthIncreasesN 3

-- Justin Le interestingly points out that part 2 can be solved in the exact same way as part 1
-- except that we drop 3 instead of 1.
-- "We can check that items three positions apart are increasing because for each window the
-- sum of the window is increasing if the new item gained is bigger than the item that was just
-- lost"
--
-- This leads to a generalized function which can work for both part 1 and 2:

getDepthIncreasesN :: Int -> [Int] -> Int
getDepthIncreasesN n depths = length $ filter (== True) $ zipWith (<) depths (drop n depths)

main :: IO ()
main = do
  depths <- readFileLines dataFilePath
  putStrLn "Part One:"
  print (day1part1 depths)

  putStrLn "Part Two:"
  print (day1part2 depths)
