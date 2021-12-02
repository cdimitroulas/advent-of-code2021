module Day1 where

dataFilePath :: String
dataFilePath = "data/day1.txt"

-- Part One
getDepthIncreases :: [Int] -> Int
getDepthIncreases [] = 0
getDepthIncreases [_] = 0
getDepthIncreases depths =
  foldl
    (\total (d1, d2) -> if d2 > d1 then total + 1 else total)
    0
    depthPairs
  where
    depthPairs = zip depths (drop 1 depths)

-- Part Two
-- This time we need to look at groups of three values instead of individual values like in
-- part 1
getTripleSums :: [Int] -> [Int]
getTripleSums depths@(d1 : d2 : d3 : _) = (d1 + d2 + d3) : getTripleSums (tail depths)
getTripleSums _ = []

getDepthIncreases' :: [Int] -> Int
getDepthIncreases' = getDepthIncreases . getTripleSums

main :: IO ()
main = do
  depths <- map read . lines <$> readFile dataFilePath
  putStrLn "Part One:"
  print (getDepthIncreases depths)

  putStrLn "Part Two:"
  print (getDepthIncreases' depths)
