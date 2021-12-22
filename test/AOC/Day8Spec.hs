module AOC.Day8Spec (spec) where

import           AOC.Challenge.Day8
import           Data.Attoparsec.Text
import qualified Data.Text.IO         as TIO
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day8-example.txt"

spec :: Spec
spec = do
  input <- runIO $ parseOnly inputParser <$> TIO.readFile exampleDataFilePath

  describe "day8part1" $ do
    it "calculates the correct result " $ do
      day8Part1 <$> input `shouldBe` Right 26

  describe "day8part2" $ do
    it "calculates the correct result " $ do
      day8Part2 <$> input `shouldBe` Right 61229
