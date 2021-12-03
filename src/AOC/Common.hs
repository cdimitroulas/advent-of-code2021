module AOC.Common where

type Filepath = String

readFileLines :: Read a => Filepath -> IO [a]
readFileLines filepath = map read . lines <$> readFile filepath

parseFileLines :: ([String] -> Maybe [a]) -> Filepath -> IO (Maybe [a])
parseFileLines parse filepath = parse . lines <$> readFile filepath
