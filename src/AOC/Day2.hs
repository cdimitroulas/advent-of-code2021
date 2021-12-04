{-# LANGUAGE RecordWildCards #-}

module AOC.Day2 (Position (..), runCommands, runCommands', parseCommand) where

import           AOC.Common
import           Data.Foldable (foldl')
import           Data.Maybe    (fromMaybe)
import           Text.Read

data Direction = Forward | Down | Up deriving (Eq, Show)

strToDir :: String -> Maybe Direction
strToDir "forward" = Just Forward
strToDir "down"    = Just Down
strToDir "up"      = Just Up
strToDir _         = Nothing

type Command = (Direction, Int)

data Position = Position
  { posHorizontal :: Int,
    posDepth      :: Int,
    -- only used in movePositionWithAim
    posAim        :: Int
  }
  deriving (Eq, Show)

movePosition :: Position -> Command -> Position
movePosition Position {..} (Forward, amt) = Position (posHorizontal + amt) posDepth 0
movePosition Position {..} (Down, amt) = Position posHorizontal (posDepth + amt) 0
movePosition Position {..} (Up, amt) = Position posHorizontal (posDepth - amt) 0

initialPosition :: Position
initialPosition = Position 0 0 0

parseCommand :: String -> Maybe Command
parseCommand txt = case words txt of
  [dir, amount] -> (,) <$> strToDir dir <*> readMaybe amount
  _             -> Nothing

runCommands :: [Command] -> Position
runCommands = foldl' movePosition initialPosition

movePositionWithAim :: Position -> Command -> Position
movePositionWithAim Position {..} (Forward, amt) =
  Position (posHorizontal + amt) (posDepth + posAim * amt) posAim
movePositionWithAim Position {..} (Down, amt) =
  Position posHorizontal posDepth (posAim + amt)
movePositionWithAim Position {..} (Up, amt) =
  Position posHorizontal posDepth (posAim - amt)

runCommands' :: [Command] -> Position
runCommands' = foldl' movePositionWithAim initialPosition

main :: IO ()
main = do
  putStrLn "Part one:"
  commands <- parseFileLines parseCommand "data/day2.txt"
  let finalPosition = runCommands $ fromMaybe [] commands
  print finalPosition
  print $ posHorizontal finalPosition * posDepth finalPosition

  putStrLn "Part two:"
  let finalPositionPart2 = runCommands' $ fromMaybe [] commands
  print finalPositionPart2
  print $ posHorizontal finalPositionPart2 * posDepth finalPositionPart2
