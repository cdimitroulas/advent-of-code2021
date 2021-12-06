{-# LANGUAGE RecordWildCards #-}

module AOC.Challenge.Day4 (day4part1, parseInput, mainDay4) where

import           AOC.Common
import           AOC.Matrix    (Matrix, Position (..))
import qualified AOC.Matrix    as Matrix
import           Data.Foldable (find, foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO

-- Bingo board is 5x5 matrix of numbers
-- More matrix operations!
data Square = Square {squareVal :: Int, squareCalled :: Bool} deriving (Eq)

squareIsCalled :: Square -> Bool
squareIsCalled = (== True) . squareCalled

type Board = Matrix Square

sumOfUnmarked :: Board -> Int
sumOfUnmarked board = sum (map sumOfUnmarkedInRow board)
  where
    sumOfUnmarkedInRow =
      foldl'
        ( \total square ->
            if not (squareCalled square)
              then total + squareVal square
              else total
        )
        0

updateBoardPosition :: (Square -> Square) -> Position -> Board -> Board
updateBoardPosition f Position {..} = setAt (setAt f posX) posY

markNumberOnBoard :: Int -> Board -> Board
markNumberOnBoard num matrix = case numPosition of
  Just pos -> updateBoardPosition markSquare pos matrix
  Nothing  -> matrix
  where
    numPosition = Matrix.findElemPosition ((== num) . squareVal) matrix
    markSquare Square {..} = Square squareVal True

boardHasBingo :: Board -> Bool
boardHasBingo board = rowHasBingo board || rowHasBingo (Matrix.invert board)
  where
    rowHasBingo =
      any
        ( (== True)
            . foldl' (\allCalled square -> allCalled && squareIsCalled square) True
        )

day4part1 :: [Board] -> [Int] -> Int
day4part1 _ [] = error "no bingo numbers provided or no board won the bingo game"
day4part1 boards (num : nums) = case boardWithBingo of
  Just board -> sumOfUnmarked board * num
  Nothing    -> day4part1 updatedBoards nums
  where
    boardWithBingo = find boardHasBingo updatedBoards
    updatedBoards = map (markNumberOnBoard num) boards

mainDay4 :: IO ()
mainDay4 = do
  (bingoNumbers, boards) <- parseInput <$> TIO.readFile "data/day4.txt"
  putStrLn "Day 4 Part one:"
  print $ day4part1 boards bingoNumbers

-- Parsing the input is different to the previous puzzles. We have a first line which is the
-- bingo numbers that are called. There is a line of whitespace after this.
-- Then we have the boards, each separated by an empty line.

parseNumbersList :: Text -> [Int]
parseNumbersList = map (read . T.unpack) . T.split (== ',')

parseBoardRow :: Text -> [Square]
parseBoardRow = map ((`Square` False) . read . T.unpack) . T.words

parseBoard :: [Text] -> Board
parseBoard = map parseBoardRow

parseInput :: Text -> ([Int], [Board])
parseInput input =
  (parseNumbersList bingoNumbers, map parseBoard boards)
  where
    -- get first line
    bingoNumbers = (head . T.lines) input
    -- get all lines after the first line and split them into separate lists at the points where
    -- there was double return character (\n\n)
    -- (which will appear as an empty string when using T.lines, hence splitList T.empty)
    boards = (tail . splitList T.empty . T.lines) input

-- Show instance for Square so we can print it nicely!
instance Show Square where
  show Square {squareVal = val, squareCalled = False} = showVal val
  show Square {squareVal = val, squareCalled = True} = "\x1b[31m" <> showVal val <> "\x1b[0m"

showVal :: Int -> String
showVal x = if length intAsStr == 2 then intAsStr <> " " else " " <> intAsStr <> " "
  where
    intAsStr = show x
