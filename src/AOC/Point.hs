{-# LANGUAGE RecordWildCards #-}

module AOC.Point (Point (..), Line (..), isHorizontalLine, isVerticalLine, toDiscretePoints) where


data Point = Point {pointX :: Int, pointY :: Int} deriving (Eq, Ord, Show)

data Line = Line {lineStart :: Point, lineEnd :: Point} deriving (Eq, Show)

isHorizontalLine :: Line -> Bool
isHorizontalLine
  Line {lineStart = Point {pointY = y1}, lineEnd = Point {pointY = y2}} = y1 == y2

isVerticalLine :: Line -> Bool
isVerticalLine
  Line {lineStart = Point {pointX = x1}, lineEnd = Point {pointX = x2}} = x1 == x2

-- Given a line returns the discrete list of points which sit along that line
-- Assumes horizontal, vertical and 45 degree diagonal lines only.
toDiscretePoints :: Line -> [Point]
toDiscretePoints line@Line {lineStart = p1, lineEnd = p2}
  | p1 == p2 = [p2]

  | isHorizontalLine line =
      p1 : toDiscretePoints
        (Line
          (Point (pointX p1 + xIncreaseFactor) (pointY p1))
          p2)

  | isVerticalLine line =
      p1 : toDiscretePoints
        (Line
          (Point (pointX p1) (pointY p1 + yIncreaseFactor))
          p2)

  -- Assumes a 45 degree horizontal line
  | otherwise =
      p1: toDiscretePoints
        (Line
          (Point (pointX p1 + xIncreaseFactor) (pointY p1 + yIncreaseFactor))
          p2)

    where
      xIncreaseFactor = if pointX p1 < pointX p2 then 1 else -1
      yIncreaseFactor = if pointY p1 < pointY p2 then 1 else -1
