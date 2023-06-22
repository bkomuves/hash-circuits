
module Vectors.Blake2 where

--------------------------------------------------------------------------------

data HashFun
  = Blake2s_256
  | Blake2b_256
--  | Blake2b_512
  deriving (Eq,Show)

--------------------------------------------------------------------------------

testVectorsKeccak :: HashFun -> [(String,String)]
testVectorsKeccak hashfun = case hashfun of
  Blake2s_256    -> blake2s_256_vectors
  Blake2b_256    -> blake2b_256_vectors

--------------------------------------------------------------------------------

infix 1 ~>
(~>) :: a -> b -> (a,b)
(~>) x y = (x,y)

--------------------------------------------------------------------------------

blake2s_256_vectors :: [(String,String)]
blake2s_256_vectors =
  [ ""          ~> "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
  , "a"         ~> "4a0d129873403037c2cd9b9048203687f6233fb6738956e0349bd4320fec3e90"
  , "abc"       ~> "508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982"
  , "foo"       ~> "08d6cad88075de8f192db097573d0e829411cd91eb6ec65e8fc16c017edfdb74"
  , "alma"      ~> "4c8d13703a43171f7effdad39f53198b6c94f813ec9d4fb220dfe03317830a63"
  , "almakorte" ~> "3fee862ff6790cdab0bb2023094003671022349e5fad110f8ef3a13c894e9ecc"
  ]

blake2b_256_vectors :: [(String,String)]
blake2b_256_vectors =
  [ ""          ~> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
  , "a"         ~> "8928aae63c84d87ea098564d1e03ad813f107add474e56aedd286349c0c03ea4"
  , "abc"       ~> "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
  , "foo"       ~> "b8fe9f7f6255a6fa08f668ab632a8d081ad87983c77cd274e48ce450f0b349fd"
  , "alma"      ~> "dabd580ce2f567a34034484016c9725a07efedaeda1108c96f2ffe5310b14cf5"
  , "almakorte" ~> "bf96756a8ec499c33ea0431ae8f6ebf6f7f91a2e2ad1846ae804aff2d82f0f2a"
  ]

--------------------------------------------------------------------------------
