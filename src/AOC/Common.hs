module AOC.Common where

type Filepath = String

-- Utility function to read a file, split the lines and read the contents into any type.
-- Could throw if the file contents do not match the expected type
readFileLines :: Read a => Filepath -> IO [a]
readFileLines filepath = map read . lines <$> readFile filepath

-- Same as readFileLines but safer as it won't throw if the file contents do not match the
-- expected type. Instead it will return Nothing if the contents can't be parsed.
parseFileLines :: (String -> Maybe a) -> Filepath -> IO (Maybe [a])
parseFileLines parse filepath = mapM parse . lines <$> readFile filepath
