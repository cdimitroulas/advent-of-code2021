module AOC.Day2Spec (spec) where

import           AOC.Common
import           AOC.Day2
import           Data.Maybe (fromMaybe)
import           Test.Syd

exampleDataFilePath :: String
exampleDataFilePath = "data/day2-example.txt"

spec :: Spec
spec = do
  commands <- runIO $ parseFileLines parseCommands exampleDataFilePath

  describe "runCommands" $ do
    it "returns the correct final position" $ do
      let finalPosition =  runCommands $ fromMaybe [] commands
      finalPosition `shouldBe` Position 15 10
      posHorizontal finalPosition * posDepth finalPosition `shouldBe` 150

