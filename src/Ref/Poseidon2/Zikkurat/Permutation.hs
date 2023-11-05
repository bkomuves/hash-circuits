
-- | The Poseidon2 permutation

module Ref.Poseidon2.Zikkurat.Permutation where

--------------------------------------------------------------------------------

import ZK.Algebra.Curves.BN128.Fr.Mont (Fr,from)

import Ref.Poseidon2.Zikkurat.RoundConsts

--------------------------------------------------------------------------------

frToInteger :: Fr -> Integer
frToInteger = ZK.Algebra.Curves.BN128.Fr.Mont.from

--------------------------------------------------------------------------------

sbox :: Fr -> Fr
sbox x = x4*x where
  x2 = x *x
  x4 = x2*x2

internalRound :: Fr -> (Fr,Fr,Fr) -> (Fr,Fr,Fr) 
internalRound c (x,y,z) = 
  ( 2*x' +   y +   z 
  ,   x' + 2*y +   z 
  ,   x' +   y + 3*z 
  )
  where
    x' = sbox (x + c) 

externalRound :: (Fr,Fr,Fr) -> (Fr,Fr,Fr) -> (Fr,Fr,Fr)
externalRound (cx,cy,cz) (x,y,z) = (x'+s , y'+s , z'+s) where
  x' = sbox (x + cx)
  y' = sbox (y + cy)
  z' = sbox (z + cz)
  s  = x' + y' + z'

linearLayer :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
linearLayer (x,y,z) = (x+s, y+s, z+s) where s = x+y+z

--------------------------------------------------------------------------------

permutation :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
permutation 
  = (\state -> foldl (flip externalRound) state finalRoundConsts   )
  . (\state -> foldl (flip internalRound) state internalRoundConsts)
  . (\state -> foldl (flip externalRound) state initialRoundConsts )
  . linearLayer

--------------------------------------------------------------------------------

-- | BN254 example test vector
exInput, exOutput :: (Fr,Fr,Fr)
exInput = (0,1,2) 
exOutput = 
  ( 0x30610a447b7dec194697fb50786aa7421494bd64c221ba4d3b1af25fb07bd103 
  , 0x13f731d6ffbad391be22d2ac364151849e19fa38eced4e761bcd21dbdc600288 
  , 0x1433e2c8f68382c447c5c14b8b3df7cbfd9273dd655fe52f1357c27150da786f 
  )

kats :: Bool  
kats = permutation exInput == exOutput 

--------------------------------------------------------------------------------
