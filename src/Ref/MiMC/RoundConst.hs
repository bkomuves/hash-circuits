
-- | Round constants for our MiMC implementation
--
-- The algorithm to generate the constants is the following:
--
-- * take the largest prefix of the digits of pi=3.1415.., interpreted as an integer,
--   which is less than `2^256`. Call this `seed`.
-- * let the i-th round constant, starting from i=0, be the SHA256 hash `H(seed|i) mod p`
--   where both `seed` and `i` and the hash are interpreted as a 256-bit little-endian 
--   numbers.
--
-- For reference:
-- 
--     seed = 31415926535897932384626433832795028841971693993751058209749445923078164062862
--          = 0x4574c8c75d6e88acd28f7e467dac97b5c60c3838d9dad993900bdf402152228e
--

module Ref.MiMC.RoundConst where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word 

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Control.Monad

import Text.Printf

import Ref.Common
import Ref.SHA2.SHA256

--------------------------------------------------------------------------------

-- | @nrounds = 2 * ceil[ log2(p) / log2(5) ]@
nrounds :: Int
nrounds = 220        

prime :: Integer
prime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

exponent :: Integer
exponent = 5

-- | @(a^5)^invExponent == a@ (modulo prime)
invExponent :: Integer
invExponent = 17510594297471420177797124596205820070838691520332827474958563349260646796493

seed :: Integer
seed = 0x4574c8c75d6e88acd28f7e467dac97b5c60c3838d9dad993900bdf402152228e

--------------------------------------------------------------------------------

toBytes :: Integer -> [Word8]
toBytes n = [ fromInteger (shiftR n (8*i) .&. 255) | i<-[0..31] ]

fromBytes :: [Word8] -> Integer
fromBytes ws = sum (zipWith f [0..] ws) where
  f i w = shiftL (fromIntegral w) (8*i)

-- | Calculates the i-th round constant. Example:
--
-- Preimage for @i=5@ is:
--
-- > 00000000  8e 22 52 21 40 df 0b 90  93 d9 da d9 38 38 0c c6 
-- > 00000010  b5 97 ac 7d 46 7e 8f d2  ac 88 6e 5d c7 c8 74 45 
-- > 00000020  05 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 
-- > 00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 
--
-- SHA256 of that is:
--
-- > 8d905fff93665a967eebeeab69caf8fa3fc170954221012b4ea72d1906d1486e
--
-- as little-endian integer, that is
--
-- > a5 = 0x6e48d106192da74e2b0121429570c13ffaf8ca69abeeeb7e965a6693ff5f908d
-- >    = 49883068962221093690836652677075821172204594681873204573268154104756989563021
--
-- so the corresponding round constant will be
--
-- > rc[5] = a5 `mod` prime 
-- >       = 6106583218542543246343841186561270995107865881041135885871745731605372571787
--
ithRoundConst :: Int -> Integer
ithRoundConst = fst . ithRoundConst'

ithRoundConst' :: Int -> (Integer,(ByteString,ByteString))
ithRoundConst' i = (rc,(bs,hash)) where
  ws = toBytes seed ++ toBytes (fromIntegral i)
  bs = B.pack ws
  digest = sha256 bs
  hash = B.concat $ map toByteStringBE $ fromDigest digest
  rc = mod (fromBytes $ B.unpack hash) prime

--------------------------------------------------------------------------------

roundConsts :: [Integer]
roundConsts = [ ithRoundConst i | i<-[0..nrounds-1] ]

showRoundConsts :: String
showRoundConsts = unlines $
  [ ("  " ++ [c] ++ " " ++ show rc) 
  | (c,rc) <- zip commas roundConsts
  ] ++
  [ "  ]" ]
  where
    commas = '[' : repeat ','

printRoundConsts :: IO ()
printRoundConsts = putStrLn showRoundConsts

exportRoundConsts :: FilePath -> IO ()
exportRoundConsts fp = writeFile fp showRoundConsts

--------------------------------------------------------------------------------
