module AOC.Day2 (Position(..), parseCommands, runCommands) where

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

data Position = Position { posHorizontal :: Int, posDepth :: Int } deriving (Eq, Show)

movePosition :: Position -> Command -> Position
movePosition Position { posHorizontal = horizontal, posDepth = vertical } (Forward, amt) = Position (horizontal + amt) vertical
movePosition Position { posHorizontal = horizontal, posDepth = vertical } (Down, amt) = Position horizontal (vertical + amt)
movePosition Position { posHorizontal = horizontal, posDepth = vertical } (Up, amt) = Position horizontal (vertical - amt)

initialPosition :: Position
initialPosition = Position 0 0

parseCommands :: [String] -> Maybe [Command]
parseCommands = mapM parseCommand
  where
    parseCommand txt = case words txt of
                         [dir, amount] -> (,) <$> strToDir dir <*> readMaybe amount
                         _ -> Nothing

runCommands :: [Command] -> Position
runCommands = foldl' movePosition initialPosition

main :: IO ()
main = do
  putStrLn "Part one:"
  commands <- parseFileLines parseCommands "data/day2.txt"
  let finalPosition = runCommands $ fromMaybe [] commands
  print finalPosition
  print $ posHorizontal finalPosition * posDepth finalPosition
