module AOC.Day5Spec (spec) where

import           AOC.Challenge.Day5   (day5part1, day5part2, parseLine)
import           Data.Attoparsec.Text
import           Data.List.NonEmpty   (fromList)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day5-example.txt"

spec :: Spec
spec = do
  ls <- runIO $ mapM (parseOnly parseLine) . T.lines <$> TIO.readFile exampleDataFilePath

  describe "day5part1" $ do
    it "calculates the correct result " $ do
      day5part1 . fromList <$> ls `shouldBe` Right 5

  describe "day5part2" $ do
    it "calculates the correct result " $ do
      day5part2 . fromList <$> ls `shouldBe` Right 12
