module AOC.Day4Spec (spec) where

import           AOC.Challenge.Day4 (day4part1, parseInput)
import qualified Data.Text.IO       as TIO
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day4-example.txt"

spec :: Spec
spec = do
  (bingoNumbers, boards) <- runIO $ parseInput <$> TIO.readFile exampleDataFilePath

  describe "day4part1" $ do
    it "calculates the correct result " $ do
      day4part1 boards bingoNumbers `shouldBe` 4512
