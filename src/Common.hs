module Common where

type Filepath = String

readFileLines :: Read a => Filepath -> IO [a]
readFileLines filepath = map read . lines <$> readFile filepath
