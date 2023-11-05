
-- | The inverse of the Poseidon2 permutation

module Ref.Poseidon2.Zikkurat.InvPerm where

--------------------------------------------------------------------------------

import ZK.Algebra.Class.Field
import ZK.Algebra.Curves.BN128.Fr.Mont (Fr)

import Ref.Poseidon2.Zikkurat.RoundConsts
import Ref.Poseidon2.Zikkurat.Permutation

--------------------------------------------------------------------------------

invPermSanityCheck :: Bool
invPermSanityCheck = invPermSanityCheck' (123456,78901234,567890123)

invPermSanityCheck' :: (Fr,Fr,Fr) -> Bool
invPermSanityCheck' xyz = (inversePermutation (permutation xyz) == xyz)

--------------------------------------------------------------------------------

-- | @(a^5)^invExponent == a@ (modulo prime)
invExponent :: Integer
invExponent = 17510594297471420177797124596205820070838691520332827474958563349260646796493

invSbox :: Fr -> Fr
invSbox x = power x invExponent

inverseInternalRound :: Fr -> (Fr,Fr,Fr) -> (Fr,Fr,Fr) 
inverseInternalRound c xyz = (u',v,w) where
  (u,v,w) = inverseInternalMatrix xyz
  u' = invSbox u - c

inverseExternalRound :: (Fr,Fr,Fr) -> (Fr,Fr,Fr) -> (Fr,Fr,Fr)
inverseExternalRound (cx,cy,cz) xyz = (u',v',w') where
  (u,v,w) = inverseExternalMatrix xyz
  u' = invSbox u - cx
  v' = invSbox v - cy
  w' = invSbox w - cz

--------------------------------------------------------------------------------

inverseInternalMatrix :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
inverseInternalMatrix (x,y,z) = (u,v,w) where
  u = plus_5over7*x + minus2over7*y + minus1over7*z
  v = minus2over7*x + plus_5over7*y + minus1over7*z
  w = minus1over7*x + minus1over7*y + plus_3over7*z

inverseExternalMatrix :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
inverseExternalMatrix = inverseLinearLayer

inverseLinearLayer :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
inverseLinearLayer (x,y,z) = (u,v,w) where
  u = plus_3over4*x + minus1over4*y + minus1over4*z
  v = minus1over4*x + plus_3over4*y + minus1over4*z
  w = minus1over4*x + minus1over4*y + plus_3over4*z

minus1over4 :: Fr
minus1over4 = - 1 / 4

plus_3over4 :: Fr
plus_3over4 = 3 / 4

minus1over7 :: Fr
minus1over7 = - 1 / 7

minus2over7 :: Fr
minus2over7 = - 2 / 7

plus_3over7 :: Fr
plus_3over7 = 3 / 7

plus_5over7 :: Fr
plus_5over7 = 5 / 7

--------------------------------------------------------------------------------

inversePermutation :: (Fr,Fr,Fr) -> (Fr,Fr,Fr)
inversePermutation 
  = inverseLinearLayer
  . (\state -> foldl (flip inverseExternalRound) state (reverse initialRoundConsts ))
  . (\state -> foldl (flip inverseInternalRound) state (reverse internalRoundConsts))
  . (\state -> foldl (flip inverseExternalRound) state (reverse finalRoundConsts   ))

--------------------------------------------------------------------------------

