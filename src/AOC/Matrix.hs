module AOC.Matrix (Matrix, getCol, invert) where

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
