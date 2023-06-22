
-- | BLAKE2s hash function, (slow) reference implementation.
--
-- See <https://datatracker.ietf.org/doc/html/rfc7693>

{-# LANGUAGE BangPatterns, TypeApplications, FlexibleInstances #-}
module Ref.Blake2.BLAKE2s where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word
import Data.Bits
import Data.Char
import Data.List hiding (partition)

import Ref.Common

--------------------------------------------------------------------------------

kk =  0       -- key bytes
nn = 32       -- output bytes

--------------------------------------------------------------------------------

paramBlock :: [Word32]
paramBlock = p0 : replicate 7 0 where
  p0 = nn + shiftL kk 8 + shiftL 0x0101 16

-- IV[i] = floor(2**w * frac(sqrt(prime(i+1)))), where prime(i)
-- is the i:th prime number ( 2, 3, 5, 7, 11, 13, 17, 19 )
-- and sqrt(x) is the square root of x.
initializationVector :: [Word32]
initializationVector = 
  [ 0x6A09E667 , 0xBB67AE85 , 0x3C6EF372 , 0xA54FF53A
  , 0x510E527F , 0x9B05688C , 0x1F83D9AB , 0x5BE0CD19
  ]

type SigmaVec = Array Int Int

sigma :: [SigmaVec]
sigma = map (listArray (0,15)) sigma'

sigma' :: [[Int]]
sigma' =
  [ [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]
  , [ 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 ]
  , [ 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 ]
  , [ 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 ]
  , [ 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 ]
  , [ 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 ]
  , [ 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 ]
  , [ 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 ]
  , [ 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 ]
  , [ 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 ]
  ]

-- an index between 0..15
type Idx = Int

-- an index between 0..7
type Jdx = Int

-- | Array of 16 words
type WorkVec  = Array Idx Word32

-- | Array of 8 words
type StateVec = Array Jdx Word32

--------------------------------------------------------------------------------

-- rotation constants
_R1 = 16
_R2 = 12
_R3 =  8
_R4 =  7

mixingFunG :: WorkVec -> (Idx,Idx,Idx,Idx) -> (Word32,Word32) -> WorkVec
mixingFunG v0 (a,b,c,d) (x,y) = {- debug "G" (toHexString $ elems v8) -} v8 where

  v1 = op1 v0
  v2 = op2 v1
  v3 = op3 v2
  v4 = op4 v3
  v5 = op5 v4
  v6 = op6 v5
  v7 = op7 v6
  v8 = op8 v7

  replace v i y = v // [(i,y)]

  op1 v = replace v a $ v!a + v!b + x              
  op2 v = replace v d $ rotateR (v!d `xor` v!a) _R1
  op3 v = replace v c $ v!c + v!d                  
  op4 v = replace v b $ rotateR (v!b `xor` v!c) _R2
  op5 v = replace v a $ v!a + v!b + y              
  op6 v = replace v d $ rotateR (v!d `xor` v!a) _R3
  op7 v = replace v c $ v!c + v!d                  
  op8 v = replace v b $ rotateR (v!b `xor` v!c) _R4

--------------------------------------------------------------------------------

-- | compression function inputs:
--
-- * h = state (8 words)
-- 
-- * m = current message block (16 words)
--
-- * t = offset
--
-- * f = final block flag
--
compressionF :: StateVec -> WorkVec -> Word64 -> Bool -> StateVec
compressionF !h !m !t !f = h' where

  v0 = listArray (0,15) (elems h ++ initializationVector)
  v1 = v0 // [ (12, v0!12 `xor` fromIntegral         t     )
             , (13, v0!13 `xor` fromIntegral (shiftR t 32) ) ]
  v  = if f then v1 // [ (14, complement (v1!14)) ]
            else v1

  rounds :: [WorkVec -> WorkVec]
  rounds = [ singleRound m i | i<-[0..9] ] 

  tenRounds :: WorkVec -> WorkVec
  tenRounds = foldr (.) id (reverse rounds)

  v' = tenRounds v

  h' = listArray (0,7) [ h!i `xor` v'!i `xor` v'!(i+8) | i<-[0..7] ]

--------------------------------------------------------------------------------

singleRound :: WorkVec -> Int -> WorkVec -> WorkVec
singleRound msg i = singleRound' msg (sigma!!i)

singleRound' :: WorkVec -> SigmaVec -> WorkVec -> WorkVec
singleRound' m s v0 = v8 where
  v1 = mixingFunG v0 (0, 4,  8, 12) ( m!(s! 0) , m!(s! 1) )
  v2 = mixingFunG v1 (1, 5,  9, 13) ( m!(s! 2) , m!(s! 3) )
  v3 = mixingFunG v2 (2, 6, 10, 14) ( m!(s! 4) , m!(s! 5) )
  v4 = mixingFunG v3 (3, 7, 11, 15) ( m!(s! 6) , m!(s! 7) )
  
  v5 = mixingFunG v4 (0, 5, 10, 15) ( m!(s! 8) , m!(s! 9) )
  v6 = mixingFunG v5 (1, 6, 11, 12) ( m!(s!10) , m!(s!11) )
  v7 = mixingFunG v6 (2, 7,  8, 13) ( m!(s!12) , m!(s!13) )
  v8 = mixingFunG v7 (3, 4,  9, 14) ( m!(s!14) , m!(s!15) )

--------------------------------------------------------------------------------

foldWithIndexAndLast :: (Int -> Bool -> a -> b -> a) -> a -> [b] -> a
foldWithIndexAndLast f = go 0 where
  go i x []     = x
  go i x [y]    =           f i True  x y  
  go i x (y:ys) = go (i+1) (f i False x y) ys

blake2s__ :: String -> String
blake2s__ = blake2s_ . map (fromIntegral . ord)

blake2s_ :: [Word8] -> String
blake2s_ = concatMap toHexStringLE . elems . blake2s

blake2s :: [Word8] -> StateVec
blake2s msg = finalHash where

  finalHash = foldWithIndexAndLast g h0 (messageToBlocks msg) 

  h0 = listArray (0,7) $ zipWith xor initializationVector paramBlock
  ll = length msg

  g i False h m = compressionF h m (fromIntegral (i+1) * 64                   ) False
  g i True  h m = compressionF h m (fromIntegral $ if kk==0 then ll else ll+64) True

--------------------------------------------------------------------------------

-- | partitions a list into blocks of the given size
partition :: Int -> a -> [a] -> [[a]]
partition k x0 = go where
  go [] = []
  go xs = case drop k xs of
    []   -> [ take k (xs ++ repeat x0) ]
    rest ->   take k  xs : go rest 

-- | converts 64 bytes to 16 little-endian words
bytesToWordBlock :: [Word8] -> WorkVec
bytesToWordBlock = listArray (0,15) . map toWord32 . partition 4 0

toWord32 :: [Word8] -> Word32
toWord32 [a,b,c,d] =         fromIntegral a 
                   + shiftL (fromIntegral b) 8 
                   + shiftL (fromIntegral c) 16
                   + shiftL (fromIntegral d) 24
toWord32 _ = error "toWord32"

-- Note:
--
-- "However, in the special case of an unkeyed empty message (kk = 0 and
--  ll = 0), we still set dd = 1 and d[0] consists of all zeros."
messageToBlocks :: [Word8] -> [WorkVec] 
messageToBlocks [] = [ listArray (0,15) (replicate 16 0) ]
messageToBlocks ws = (map bytesToWordBlock . partition 64 0) ws

--------------------------------------------------------------------------------

{-
testinput = "abc"
-- testinput = "abcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijklabcdefghijkl"
-- testinput = "abcdefghijklmnopabcdefghijklmnopabcdefghijklmnopabcdefghijklmnopabcdefghijklmnop"

testmain = do
  let input_str  = testinput 
  let input = map (fromIntegral . ord) input_str
  let !hash = blake2s_ input
  putStrLn $ "input len  = " ++ show (length input_str)
  putStrLn $ "input      = " ++ show input_str
  putStrLn $ "final hash = " ++ hash
-}
