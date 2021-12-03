module Test.Day1 where

import           Day1
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day1-example.txt"

day1Spec :: Spec
day1Spec = do
  depths <- runIO $ map read . lines <$> readFile exampleDataFilePath

  describe "getDepthIncreases" $ do
    it "returns the correct number of depth increases for the example data" $ do
      getDepthIncreases depths == 7

  describe "getDepthIncreases'" $ do
    it "returns the correct number of depths increases for the example data" $ do
      getDepthIncreases' depths == 5
