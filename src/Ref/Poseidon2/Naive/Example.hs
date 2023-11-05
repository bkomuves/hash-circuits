
module Ref.Poseidon2.Naive.Example where

--------------------------------------------------------------------------------

import Ref.Poseidon2.Naive.Permutation
import Ref.Poseidon2.Naive.BN256

--------------------------------------------------------------------------------

-- BN256 example test vector
exInput, exOutput :: Triple BN256
exInput = (0,1,2) 
exOutput = 
  ( 0x30610a447b7dec194697fb50786aa7421494bd64c221ba4d3b1af25fb07bd103 
  , 0x13f731d6ffbad391be22d2ac364151849e19fa38eced4e761bcd21dbdc600288 
  , 0x1433e2c8f68382c447c5c14b8b3df7cbfd9273dd655fe52f1357c27150da786f 
  )

kats :: Bool  
kats = permutation exInput == exOutput 

--------------------------------------------------------------------------------
