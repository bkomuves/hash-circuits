
-- | The bn256 (aka bn128 aka BN254) scalar field

module Ref.Poseidon2.BN256 where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Ratio

import qualified Ref.Poseidon2.PrimeField as Gen

--------------------------------------------------------------------------------

-- | BN256 scalar field
newtype BN256 = MkBN256 Integer deriving (Eq,Show)

unBN256 :: BN256 -> Integer
unBN256 (MkBN256 x) = x

toBN256 :: Integer -> BN256
toBN256 = MkBN256 . modBN256

modBN256 :: Integer -> Integer
modBN256 x = mod x fieldPrimeBN256

fieldPrimeBN256 :: Integer
fieldPrimeBN256 = 21888242871839275222246405745257275088548364400416034343698204186575808495617

instance Num BN256 where
  fromInteger = toBN256 . fromInteger
  negate (MkBN256 x)             = MkBN256 $ Gen.neg fieldPrimeBN256 x
  (+)    (MkBN256 x) (MkBN256 y) = MkBN256 $ Gen.add fieldPrimeBN256 x y
  (-)    (MkBN256 x) (MkBN256 y) = MkBN256 $ Gen.sub fieldPrimeBN256 x y
  (*)    (MkBN256 x) (MkBN256 y) = MkBN256 $ Gen.mul fieldPrimeBN256 x y  
  abs    x = x
  signum _ = toBN256 1  

instance Fractional BN256 where
  recip (MkBN256 x)             = MkBN256 $ Gen.inv fieldPrimeBN256 x
  (/)   (MkBN256 x) (MkBN256 y) = MkBN256 $ Gen.div fieldPrimeBN256 x y
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)  

--------------------------------------------------------------------------------
