module AOC.Day3Spec (spec) where

import           AOC.Common
import           AOC.Day3
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day3-example.txt"

spec :: Spec
spec = do
  describe "binToDecimal" $ do
    it "correctly converts binary to decimal" $ do
      binToDecimal <$> parseBinary "0" `shouldBe` Just 0
      binToDecimal <$> parseBinary "1" `shouldBe` Just 1
      binToDecimal <$> parseBinary "10" `shouldBe` Just 2
      binToDecimal <$> parseBinary "100" `shouldBe` Just 4
      binToDecimal <$> parseBinary "101" `shouldBe` Just 5
      binToDecimal <$> parseBinary "11011" `shouldBe` Just 27
      binToDecimal <$> parseBinary "100101011100" `shouldBe` Just 2396

  binaries <- runIO $ parseFileLines parseBinary exampleDataFilePath

  describe "calcPowerConsumption" $ do
    it "calculates the correct power consumption" $ do
      calcPowerConsumption <$> binaries `shouldBe` Just 198

  describe "calcLifeSupportRating" $ do
    it "calculates the correct life support rating" $ do
      calcLifeSupportRating <$> binaries `shouldBe` Just 230
