module AOC.Matrix (Matrix, getCol, invert, prettyPrint, updateRow, updateCol) where

import           AOC.Common
import           Data.List  (foldl')

type Matrix a = [[a]]

-- Inverts the rows and columns. Useful when you want to perform operations on each column
invert :: Matrix a -> Matrix a
invert matrix = run 0 matrix
  where
    run index mat =
      if index < length (head matrix)
      then getCol index matrix : run (index + 1) mat
      else []

getCol :: Int -> Matrix a -> [a]
getCol pos = map (!! pos)

getRow :: Int -> Matrix a -> [a]
getRow pos = (!! pos)

updateRow :: ([a] -> [a]) -> Int -> Matrix a -> Matrix a
updateRow = setAt

updateCol :: ([a] -> [a]) -> Int -> Matrix a -> Matrix a
updateCol f pos = invert . updateRow f pos . invert

prettyPrint :: Show a => Matrix a -> String
prettyPrint []     = ""
prettyPrint (x:xs) =
  foldl' (\str item -> str <> mkSpacer (show item) <> show item) "" x <> "\n" <> prettyPrint xs
  where
    mkSpacer item = if length item == 2 then " " else "  "
