{-# LANGUAGE RecordWildCards #-}

module AOC.Day3 (parseBinary, binToDecimal, calcPowerConsumption, calcLifeSupportRating) where

import           AOC.Common
import qualified AOC.Matrix    as Matrix
import           Data.Foldable (foldl')
import           Data.Maybe    (fromMaybe)

data Bit = Zero | One deriving (Eq)

type Binary = [Bit]

instance Show Bit where
  show Zero = "0"
  show One  = "1"

instance Ord Bit where
  Zero <= One = True
  Zero <= Zero = True
  One <= Zero = False
  One <= One = True

bitToInteger :: Bit -> Int
bitToInteger Zero = 0
bitToInteger One  = 1

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

parseBit :: Char -> Maybe Bit
parseBit '0' = Just Zero
parseBit '1' = Just One
parseBit _   = Nothing

parseBinary :: String -> Maybe Binary
parseBinary = mapM parseBit

binToDecimal :: Binary -> Int
binToDecimal binary = fst (foldl' addBit (0, 0) (reverse binary))
  where
    addBit :: (Int, Int) -> Bit -> (Int, Int)
    addBit (decimal, e) bitVal = (decimal + (2 ^ e * bitToInteger bitVal), e + 1)

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
calcPowerConsumption binaries = binToDecimal gammaRate * binToDecimal epsilonRate
  where
    gammaRate = getGammaRate binaries
    epsilonRate = map flipBit gammaRate

-- Given a list of binaries, returns the most common bit in a particular position.
-- For example, for position 0 it would look at all the first bits in all the binaries.
mostCommonBitInPosition :: Int -> [Binary] -> Bit
mostCommonBitInPosition pos = mostCommonBit . Matrix.getCol pos

-- We need to filter the numbers by whichever bit is most common in each column
-- In order to do so we need to increment the position of the bit we are inspecting in each
-- binary each time we loop
getOxygenGenRating :: [Binary] -> Binary
getOxygenGenRating = run 0
  where
    run _ [binary] = binary
    run position bins =
      run (position + 1) $
        filter ((== mostCommonBitInPosition position bins) . (!! position)) bins

-- getCO2ScrubberRating is the same as getOxygenGenRating except that we look for the least
-- common bit in each column (/= mostCommonBitVal)
getCO2ScrubberRating :: [Binary] -> Binary
getCO2ScrubberRating = run 0
  where
    run _ [binary] = binary
    run position bins =
      run (position + 1) $
        filter ((/= mostCommonBitInPosition position bins) . (!! position))
          bins

calcLifeSupportRating :: [Binary] -> Int
calcLifeSupportRating binaries = binToDecimal oxygenGenRating * binToDecimal co2ScrubberRating
  where
    oxygenGenRating = getOxygenGenRating binaries
    co2ScrubberRating = getCO2ScrubberRating binaries

main :: IO ()
main = do
  binaries <- fromMaybe [] <$> parseFileLines parseBinary "data/day3.txt"

  putStrLn "Part one:"
  print $ calcPowerConsumption binaries

  putStrLn "Part two:"
  print $ calcLifeSupportRating binaries
