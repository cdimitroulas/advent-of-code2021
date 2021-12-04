module AOC.Binary (Bit (..), Binary, parseBit, parseBinary, binToDecimal, flipBit) where

import           Data.Foldable (foldl')

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
