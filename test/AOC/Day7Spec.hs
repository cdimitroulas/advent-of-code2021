module AOC.Day7Spec (spec) where

import           AOC.Challenge.Day7
import           Data.Attoparsec.Text
import qualified Data.Text.IO         as TIO
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day7-example.txt"

spec :: Spec
spec = do
  crabs <- runIO $ parseOnly parseInput <$> TIO.readFile exampleDataFilePath

  describe "day7part1" $ do
    it "calculates the correct result " $ do
      day7part1 <$> crabs `shouldBe` Right 37

  describe "day7part2" $ do
    it "calculates the correct result " $ do
      day7part2 <$> crabs `shouldBe` Right 168
