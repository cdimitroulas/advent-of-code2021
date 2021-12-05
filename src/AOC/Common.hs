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

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = run 0
  where
    run _ []         = []
    run index (x:xs) = f index x : run (index + 1) xs

setAt :: (a -> a) -> Int -> [a] -> [a]
setAt f pos = mapWithIndex (\index -> if index == pos then f else id)

-- Repeatedly splits a list by the provided separator and collects the results
splitList :: Eq a => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep list = h : splitList sep t
  where
    (h, t) = split (== sep) list

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left, right)
  where
    (left, right') = break f s
    right = if null right' then [] else tail right'
