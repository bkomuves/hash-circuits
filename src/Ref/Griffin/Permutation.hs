
-- | The Griffin permutation

{-# LANGUAGE BangPatterns #-}
module Ref.Griffin.Permutation where

--------------------------------------------------------------------------------

import Data.Array
import Data.List

import ZK.Algebra.Class.Field
import ZK.Algebra.Curves.BN128.Fr.Mont (Fr,from)

--------------------------------------------------------------------------------

-- used in the tests
frToInteger :: Fr -> Integer
frToInteger = asInteger

--------------------------------------------------------------------------------

type State = (Fr,Fr,Fr)

pow5 :: Fr -> Fr
pow5 a = a*a4 where
  a2 = square a
  a4 = square a2

powInv5 :: Fr -> Fr
powInv5 a = power a expo_inv

sanityCheckExpoInv =
  [ 13 - pow5 (powInv5 13)
  , 17 - powInv5 (pow5 17)
  ]

sbox :: State -> State
sbox (x,y,z) = (x',y',z') where
  x' = powInv5 x
  y' = pow5    y
  z' = z * (square u + alpha * u + beta)
  u  = x' + y'

addRC :: Int -> State -> State
addRC 0 = id
addRC i = let (a,b,c) = roundConstants!(i-1) in \(x,y,z) -> (x+a, y+b, z+c)

linear :: State -> State
linear (x,y,z) = let s = x+y+z in (x+s, y+s, z+s)

singleRound :: Int -> State -> State
singleRound i = linear . sbox . addRC i

permute :: State -> State
permute input = foldl' (flip singleRound) (linear input) [0..nrounds-1]

compress :: Fr -> Fr -> Fr 
compress x y = case permute (x,y,0) of (z,_,_) -> z

--------------------------------------------------------------------------------

iter :: Int -> (a -> a) -> a -> a
iter n f = go n where
  go  0 !x = x
  go !n !x = go (n-1) (f x)

--------------------------------------------------------------------------------
-- parameters from <https://extgit.isec.tugraz.at/krypto/zkfriendlyhashzoo>

-- the permutation of (0,1,2)
kat :: State
kat = 
  ( 0x2311cdb3076c3a7ee37fd5a271e0f3a8a3cc38057d0cea37b78951f43b1b6ff6
  , 0x1d3aaed9ea361e899e667abd18e5328555b97b5c3890d52b261f940d6ab4df58
  , 0x22614a0ac719cb623a636adac3bac1b85b5a7a418fcf8ab3a3ae0787fb4bed9d
  )

nrounds  = 12 :: Int
expo_inv = 0x26b6a528b427b35493736af8679aad17535cb9d394945a0dcfe7f7a98ccccccd :: Integer
alpha    = 0x146ecffb34a66316fae66609f78d1310bc14ad7208082ca7943afebb1da4aa4a :: Fr
beta     = 0x2b568115d544c7e941eff6ccc935384619b0fb7d2c5ba6c078c34cf81697ee1c :: Fr

roundConstants :: Array Int State
roundConstants = listArray (0,10)
  [ ( 0x2fb30cafdb1f76156dfabf0cd0af4b895e764ac2a84386c9d0d7aed6a7f4eac9 
    , 0x282927892ce324572f19abb14871d2b539a80d8a5800cdb87a81e1697a94b6c9 
    , 0x03d0f3f2711dd59e3d97fc797261300cd3fee33b95cf710a32edf42aa2bc0905 )
  , ( 0x036a8b3eb9ef35c74ea5a367ed279ee6d043d4ff69817f192c7251b91dcbb03d 
    , 0x2a626d396e7fa8ce8d6339bb37bd48491d56db0c7ac0afb5008a7464d5776a26 
    , 0x0cc9dfabbeaef7982543453ea3ac37ef2bfefd35a7e7070aa39b021035852d5b )
  , ( 0x2a1951149e2568ab28e972a2ceddc49eff0cae8e1cddcf4b0684a73a1b4ef61b 
    , 0x2d0ff8e9158b2fd7ae3afe01cf09d4ce9ff81e6127e441eb6cbc79d21f22be9e 
    , 0x1cc315b7ea0c1efb538f0c3248a7da062309a9e41af5a555c9ea9e8a10930cb5 )
  , ( 0x03cb10093ea62fb3f6e5680a128d07112ee566f1b424558f2ec9d86892e13a80 
    , 0x12e7bb50ae7e9e90f1765c073eb61c4be4956c424930233ce497d2722a458868 
    , 0x006b1367547937ae71e2e9b55d2f90c90131f9e6784ce3de0eb314ec748871e7 )
  , ( 0x1ffff572c53442c58809aeca02287839b11df1420deb0e99fde2baad8b86fa9c 
    , 0x13aefd685e7739f9a8b4ccdbfc5ef9e566149af4d54d6b746058ea44cb422840 
    , 0x1ea6c3ea93fe6f4ed0186941650de76ff94ab0e6e8a583996b67ba026dd2b7a5 )
  , ( 0x288f120288f9225643de833c5c15e22aadd358132bbdc12c75109048a158c9f4 
    , 0x0f638114cd7c781ab299e5233338b00cf2996df962347a00146a22103d9ad91a 
    , 0x14eeca5fa2c18999ea25ddf44237d6ac3cb8757ea452f67e2590a46f7d5b1e4f )
  , ( 0x102d1a099e8cd107dc056e72370e340b0316d237b72d99ef6261761f7eb2d61c 
    , 0x0ef741fc2fcda50f207c759dbd844a4d630cc0e4062ca80f3ffba2cce2d3f51d 
    , 0x0989b9f642485692a1f91a4b207db64f38ae545bf3e0622f3862967d27f563db )
  , ( 0x1eb4d812c80ce04784a80c89fbcc5aab89db274c62602bdd30f3223655e6cf8a 
    , 0x0124a9400253731facd46e21f41016aed69a79087f81665bc5d29a34e4e924dd 
    , 0x2520bfa6b70e6ba7ad380aaf9015b71983868a9c53e66e685ed6e48692c185a8 )
  , ( 0x1bd62b5bfa02667ac08d51d9e77bb3ab8dbd19e7a701442a20e23f7d3d6b28b4 
    , 0x1ae2f0d09fffc6bb869ebc639484a7c2084cfa3c1f88a7440713b1b154e5f952 
    , 0x0cd06e16a0d570c3799d800d92a25efbd44a795ed5b9114a28f5f869a57d9ba1 )
  , ( 0x00691740e313922521fe8c4843355eff8de0f93d4f62df0fe48755b897881c39 
    , 0x19903aa449fe9c27ee9c8320e6915b50c2822e61ce894be72b47a449c5705762 
    , 0x126e801aae44016a35deceaa3eba6ccc341fa3c2a65ab3d021fcd39abd170e1b )
  , ( 0x1b0a98be27b54ac9d5d72b94187c991c1872cb2c7777c0e880f439c133971e8d
    , 0x1e10a35afda2e5a173d4f3edecf29dacf51d8fac33d6bfb4088cc787ec647605
    , 0x1793cda85abe2782ea8e911ce92bab59a8c68e0dd561a57b064bb233f109cc57 )
  ]

--------------------------------------------------------------------------------
