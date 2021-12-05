module AOC.Day1Spec (spec) where

import           AOC.Challenge.Day1
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day1-example.txt"

spec :: Spec
spec = do
  depths <- runIO $ map read . lines <$> readFile exampleDataFilePath

  describe "day1part1" $ do
    it "returns the correct number of depth increases for the example data" $ do
      day1part1 depths == 7

  describe "day1part2" $ do
    it "returns the correct number of depths increases for the example data" $ do
      day1part2 depths == 5
