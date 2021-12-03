module AOC.Day1Spec (spec) where

import           AOC.Day1
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day1-example.txt"

spec :: Spec
spec = do
  depths <- runIO $ map read . lines <$> readFile exampleDataFilePath

  describe "getDepthIncreases" $ do
    it "returns the correct number of depth increases for the example data" $ do
      getDepthIncreases depths == 7

  describe "getDepthIncreases'" $ do
    it "returns the correct number of depths increases for the example data" $ do
      getDepthIncreases' depths == 5
