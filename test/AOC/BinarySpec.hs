module AOC.BinarySpec (spec) where

import           AOC.Binary
import           Test.Syd

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
