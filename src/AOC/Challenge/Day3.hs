{-# LANGUAGE RecordWildCards #-}

module AOC.Challenge.Day3 (calcPowerConsumption, calcLifeSupportRating) where

import           AOC.Binary    (Binary, Bit (..))
import qualified AOC.Binary    as B
import           AOC.Common
import qualified AOC.Matrix    as Matrix
import           Data.Foldable (foldl')
import           Data.Maybe    (fromMaybe)

data BitCounter = BitCounter {bitCountZero :: Int, bitCountOne :: Int} deriving (Show)

getBitCounts :: [Bit] -> BitCounter
getBitCounts =
  foldl'
    ( \BitCounter {..} bit -> case bit of
        Zero -> BitCounter (bitCountZero + 1) bitCountOne
        One  -> BitCounter bitCountZero (bitCountOne + 1)
    )
    (BitCounter 0 0)

-- The first Bit argument decides which bit takes precedence in case the number of Zeros and
-- Ones are equally numbered
mostCommonBit :: [Bit] -> Bit
mostCommonBit binary = case zeroToOneOrdering binary of
  LT -> One
  GT -> Zero
  EQ -> One
  where
    zeroToOneOrdering = (\BitCounter {..} -> compare bitCountZero bitCountOne) . getBitCounts

-- We essentially have a matrix. A list of lists (Binary is just a list of Bits).
-- We need to read the matrix vertically and perform an operation on each column to
-- get the most common bit.
getGammaRate :: [Binary] -> Binary
getGammaRate = map mostCommonBit . Matrix.invert

calcPowerConsumption :: [Binary] -> Int
calcPowerConsumption binaries = B.binToDecimal gammaRate * B.binToDecimal epsilonRate
  where
    gammaRate = getGammaRate binaries
    epsilonRate = map B.flipBit gammaRate

-- Given a list of binaries, returns the most common bit in a particular position.
-- For example, for position 0 it would look at all the first bits in all the binaries.
mostCommonBitInPosition :: Int -> [Binary] -> Bit
mostCommonBitInPosition pos = mostCommonBit . Matrix.getCol pos

data Criteria = MostCommon | LeastCommon

-- Generalized function which can be used for both oxygen generator rating and CO2 scrubber
-- rating
getRating :: Criteria -> [Binary] -> Binary
getRating criteria = run 0
  where
    comparator = case criteria of
                   MostCommon  -> (==)
                   LeastCommon -> (/=)

    run _ [binary] = binary
    run position bins =
      run (position + 1) $
        filter (comparator (mostCommonBitInPosition position bins) . (!! position)) bins


-- We need to filter the numbers by whichever bit is most common in each column
-- In order to do so we need to increment the position of the bit we are inspecting in each
-- binary each time we loop
getOxygenGenRating :: [Binary] -> Binary
getOxygenGenRating = getRating MostCommon

-- getCO2ScrubberRating is the same as getOxygenGenRating except that we look for the least
-- common bit in each column (/= mostCommonBitVal)
getCO2ScrubberRating :: [Binary] -> Binary
getCO2ScrubberRating = getRating LeastCommon

calcLifeSupportRating :: [Binary] -> Int
calcLifeSupportRating binaries = B.binToDecimal oxygenGenRating * B.binToDecimal co2ScrubberRating
  where
    oxygenGenRating = getOxygenGenRating binaries
    co2ScrubberRating = getCO2ScrubberRating binaries

main :: IO ()
main = do
  binaries <- fromMaybe [] <$> parseFileLines B.parseBinary "data/day3.txt"

  putStrLn "Part one:"
  print $ calcPowerConsumption binaries

  putStrLn "Part two:"
  print $ calcLifeSupportRating binaries
