module AOC.Day3Spec (spec) where

import           AOC.Binary
import           AOC.Challenge.Day3
import           AOC.Common
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day3-example.txt"

spec :: Spec
spec = do
  binaries <- runIO $ parseFileLines parseBinary exampleDataFilePath

  describe "calcPowerConsumption" $ do
    it "calculates the correct power consumption" $ do
      calcPowerConsumption <$> binaries `shouldBe` Just 198

  describe "calcLifeSupportRating" $ do
    it "calculates the correct life support rating" $ do
      calcLifeSupportRating <$> binaries `shouldBe` Just 230
