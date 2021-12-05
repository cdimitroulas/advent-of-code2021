module AOC.MatrixSpec (spec) where

import           AOC.Matrix
import           Test.Syd

spec :: Spec
spec = do
  describe "invert" $ do
    it "inverts the matrix" $ do
      invert ([
          [1, 2, 3, 4],
          [5, 6, 7, 8],
          [9, 10, 11, 12]
        ] :: Matrix Integer) `shouldBe`
          [
            [1, 5, 9],
            [2, 6, 10],
            [3, 7, 11],
            [4, 8, 12]
          ]

      invert ([
          [1, 2, 3, 4, 5],
          [5, 6, 7, 8, 9],
          [10, 11, 12, 13, 14]
        ] :: Matrix Integer) `shouldBe`
        [
          [1, 5, 10],
          [2, 6, 11],
          [3, 7, 12],
          [4, 8, 13],
          [5, 9, 14]
        ]

  describe "updateRow" $ do
    it "updates the row correctly" $ do
      updateRow (map (+1)) 1 ([
          [1, 2, 3, 4],
          [5, 6, 7, 8],
          [9, 10, 11, 12]
        ] :: Matrix Integer) `shouldBe`
          [
            [1, 2, 3, 4],
            [6, 7, 8, 9],
            [9, 10, 11, 12]
          ]

  describe "updateCol" $ do
    it "updates the column correctly" $ do
      updateCol (map (+1)) 2 ([
          [1, 2, 3, 4],
          [5, 6, 7, 8],
          [9, 10, 11, 12]
        ] :: Matrix Integer) `shouldBe`
          [
            [1, 2, 4, 4],
            [5, 6, 8, 8],
            [9, 10, 12, 12]
          ]
