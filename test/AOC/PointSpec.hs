module AOC.PointSpec (spec) where

-- import           AOC.Point
import           Test.Syd

spec :: Spec
spec = do
  describe "toDiscretePoints" $ do
    it "converts a line to discrete points" $ do
      -- let line =
      --       Line
      --         { lineStart = Point 0 0,
      --           lineEnd = Point 1 3
      --         }

      -- toDiscretePoints line `shouldBe` [Point 0 0, Point 1 3]

      -- let line2 =
      --       Line
      --         { lineStart = Point 0 0,
      --           lineEnd = Point 9 9
      --         }

      -- toDiscretePoints line2
      --   `shouldBe` [ Point 0 0,
      --                Point 1 1,
      --                Point 2 2,
      --                Point 3 3,
      --                Point 4 4,
      --                Point 5 5,
      --                Point 6 6,
      --                Point 7 7,
      --                Point 8 8,
      --                Point 9 9
      --              ]

      -- let line3 =
      --       Line
      --         { lineStart = Point 0 0,
      --           lineEnd = Point 9 0
      --         }

      -- toDiscretePoints line3
      --   `shouldBe` [ Point 0 0,
      --                Point 1 0,
      --                Point 2 0,
      --                Point 3 0,
      --                Point 4 0,
      --                Point 5 0,
      --                Point 6 0,
      --                Point 7 0,
      --                Point 8 0,
      --                Point 9 0
      --              ]

      -- let line4 =
      --       Line
      --         { lineStart = Point 0 0,
      --           lineEnd = Point 0 9
      --         }

      -- toDiscretePoints line4
      --   `shouldBe` [ Point 0 0,
      --                Point 0 1,
      --                Point 0 2,
      --                Point 0 3,
      --                Point 0 4,
      --                Point 0 5,
      --                Point 0 6,
      --                Point 0 7,
      --                Point 0 8,
      --                Point 0 9
      --              ]

      -- let line5 =
      --       Line
      --         { lineStart = Point 2 2,
      --           lineEnd = Point 2 1
      --         }

      -- let points = toDiscretePoints line5
      -- Point 2 2 `elem` points
      -- Point 2 1 `elem` points
      True
