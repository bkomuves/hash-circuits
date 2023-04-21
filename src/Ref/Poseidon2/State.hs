
module Ref.Poseidon2.State where

--------------------------------------------------------------------------------

import Ref.Poseidon2.BN256

--------------------------------------------------------------------------------

type Triple a = (a,a,a)

type State = Triple BN256
