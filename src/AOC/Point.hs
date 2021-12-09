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

pointIsOnLine :: Point -> Line -> Bool
pointIsOnLine p line
  | isVerticalLine line =
    pointX p == (pointX . lineStart) line
      && pointX p >= (pointX . lineStart) line
      && pointX p <= (pointX . lineEnd) line
  | isHorizontalLine line = pointY p == (pointY . lineStart) line
      && pointY p >= (pointY . lineStart) line
      && pointY p <= (pointY . lineEnd) line
  | otherwise =
    pointY p == slope * pointX p + intercept
      && pointX p >= x1
      && pointX p <= x2
      && pointY p >= y1
      && pointY p <= y2
  where
    intercept = y1 - slope * x1
    slope = (y2 - y1) `div` (x2 - x1)
    x1 = (pointX . lineStart) line
    x2 = (pointX . lineEnd) line
    y1 = (pointY . lineStart) line
    y2 = (pointY . lineEnd) line

-- Given a line returns the discrete list of points which sit along that line
toDiscretePoints :: Line -> [Point]
toDiscretePoints line@Line {lineStart = p1, lineEnd = p2} =
  [ Point x y
    | -- decreasing range doesn't work with two values like [9..0], it returns []
      -- So we need to check which value is greater and construct the range appropriately
      x <- if pointX p1 < pointX p2 then [pointX p1 .. pointX p2] else [pointX p2 .. pointX p1],
      y <- if pointY p1 < pointY p2 then [pointY p1 .. pointY p2] else [pointY p2 .. pointY p1],
      pointIsOnLine (Point x y) line
  ]
