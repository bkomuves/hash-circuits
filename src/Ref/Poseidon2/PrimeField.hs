
-- | Prime fields without type safety, naive implementation using 'Integer'-s
--
-- This module is considered internal.


{-# OPTIONS_HADDOCK hide #-} 
{-# LANGUAGE BangPatterns, DataKinds, KindSignatures #-}
module Ref.Poseidon2.PrimeField where

--------------------------------------------------------------------------------

import Data.Bits

--------------------------------------------------------------------------------

type P = Integer
type F = Integer

neg :: P -> F -> F
neg !p !x = if x == 0 then x else (p - x)

add :: P -> F -> F -> F
add !p !x !y = let a = x + y in if a < p then a else (a - p)

sub :: P -> F -> F -> F
sub !p !x !y = let a = x - y in if a >= 0 then a else (a + p)

mul :: P -> F -> F -> F
mul !p !x !y = mod (x*y) p

--------------------------------------------------------------------------------
-- * Nontrivial operations

pow :: P -> F -> Integer -> F
pow p z e 
  | e < 0     = pow p (inv p z) (negate e)
  | z == 0    = 0
  | e == 0    = 1
  | e >= pm1  = go 1 z (mod e pm1)
  | otherwise = go 1 z e
  where
    pm1 = p - 1

    go :: F -> F -> Integer -> F
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go        acc    (mul p y y) (shiftR e 1)
        _ -> go (mul p acc y) (mul p y y) (shiftR e 1)

-- | Inversion (using Euclid's algorithm)
inv :: P -> F -> F
inv !p !a 
  | a == 0    = error "field inverse of zero (generic prime)"
  | otherwise = (euclid p 1 0 a p) 

-- | Division via Euclid's algorithm
div :: P -> F -> F -> F
div !p !a !b
  | b == 0    = error "field division by zero (generic prime)"
  | otherwise = (euclid p a 0 b p) 

-- | Division via multiplying by the inverse
div2 :: P -> F -> F -> F
div2 !p !a !b = mul p a (inv p b)

--------------------------------------------------------------------------------
-- * Euclidean algorithm

-- | Extended binary Euclidean algorithm
euclid :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 
euclid !p !x1 !x2 !u !v = go x1 x2 u v where

  halfp1 = shiftR (p+1) 1

  modp :: Integer -> Integer
  modp n = mod n p

  -- Inverse using the binary Euclidean algorithm 
  euclid :: Integer -> Integer
  euclid a 
    | a == 0     = 0
    | otherwise  = go 1 0 a p
  
  go :: Integer -> Integer -> Integer -> Integer -> Integer
  go !x1 !x2 !u !v 
    | u==1       = x1
    | v==1       = x2
    | otherwise  = stepU x1 x2 u v

  stepU :: Integer -> Integer -> Integer -> Integer -> Integer
  stepU !x1 !x2 !u !v = if even u 
    then let u'  = shiftR u 1
             x1' = if even x1 then shiftR x1 1 else shiftR x1 1 + halfp1
         in  stepU x1' x2 u' v
    else     stepV x1  x2 u  v

  stepV :: Integer -> Integer -> Integer -> Integer -> Integer
  stepV !x1 !x2 !u !v = if even v
    then let v'  = shiftR v 1
             x2' = if even x2 then shiftR x2 1 else shiftR x2 1 + halfp1
         in  stepV x1 x2' u v' 
    else     final x1 x2  u v

  final :: Integer -> Integer -> Integer -> Integer -> Integer
  final !x1 !x2 !u !v = if u>=v

    then let u'  = u-v
             x1' = if x1 >= x2 then modp (x1-x2) else modp (x1+p-x2)               
         in  go x1' x2  u' v 

    else let v'  = v-u
             x2' = if x2 >= x1 then modp (x2-x1) else modp (x2+p-x1)
         in  go x1  x2' u  v'

--------------------------------------------------------------------------------
