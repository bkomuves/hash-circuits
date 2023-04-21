
module Test.Misc where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import R1CS

--------------------------------------------------------------------------------

word32ToBitsBE :: Word32 -> [Bit]
word32ToBitsBE = reverse . word32ToBitsLE

word32ToBitsLE :: Word32 -> [Bit]
word32ToBitsLE = go 32 where
  go 0 _ = []
  go k w = this : go (k-1) (shiftR w 1) where
    this = case (w .&. 1) of { 0 -> Zero ; 1 -> One }

word32FromBitsBE :: [Bit] -> Word32
word32FromBitsBE = go 0 where
  go acc []     = acc
  go acc (b:bs) = go acc' bs where acc' = 2*acc + case b of { Zero -> 0 ; One -> 1 }

word32FromBitsLE :: [Bit] -> Word32
word32FromBitsLE = go 1 where
  go mult []     = 0
  go mult (b:bs) = go (2*mult) bs + case b of { Zero -> 0 ; One -> mult }

--------------------------------------------------------------------------------

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition m xs = take m xs : partition m (drop m xs)

--------------------------------------------------------------------------------

integerToBit :: Integer -> Bit
integerToBit 0 = Zero
integerToBit 1 = One
integerToBit _ = error "integerToBit: expecting 0 or 1"

bitToInteger :: Bit -> Integer
bitToInteger Zero = 0
bitToInteger One  = 1

--------------------------------------------------------------------------------
