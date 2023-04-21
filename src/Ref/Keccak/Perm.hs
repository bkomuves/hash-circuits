
-- | Reference implementation of the Keccak-p[1600] permutations

module Ref.Keccak.Perm where

--------------------------------------------------------------------------------

import Data.Array hiding (index)
import Data.Word
import Data.Bits
import Data.List

--------------------------------------------------------------------------------
-- debugging

ex :: State
ex = listArray (0,24) [101..125]

printState :: State -> IO ()
printState arr = mapM_ print $ elems arr

--------------------------------------------------------------------------------

xors :: [Word64] -> Word64
xors = foldl1' xor

ors :: [Word64] -> Word64
ors = foldl1' (.|.)

--------------------------------------------------------------------------------

iter :: Int -> (a -> a) -> a -> a
iter 0 _ = id
iter k f = iter (k-1) f . f

smallRC :: Int -> Word64
smallRC t0 = (iter (mod t0 255) step 1) .&. 1 where 
  step :: Word64 -> Word64
  step r = 
    case r' .&. 0x100 of
      0 ->  r'             .&. 0xff
      _ -> (r' `xor` 0x71) .&. 0xff 
    where 
      r' = shiftL r 1  

capitalRC :: Int -> Word64
capitalRC idx = ors [ shiftL (smallRC (j+7*idx)) (2^j-1) | j<-[0..7] ]

roundConstants24 :: Array Int Word64
roundConstants24 = listArray (0,23) [ capitalRC i | i<-[0..23] ]

roundConstants24_ref :: Array Int Word64
roundConstants24_ref = listArray (0,23)
  [ 0x0000000000000001, 0x0000000000008082, 0x800000000000808a
  , 0x8000000080008000, 0x000000000000808b, 0x0000000080000001
  , 0x8000000080008081, 0x8000000000008009, 0x000000000000008a
  , 0x0000000000000088, 0x0000000080008009, 0x000000008000000a
  , 0x000000008000808b, 0x800000000000008b, 0x8000000000008089
  , 0x8000000000008003, 0x8000000000008002, 0x8000000000000080
  , 0x000000000000800a, 0x800000008000000a, 0x8000000080008081
  , 0x8000000000008080, 0x0000000080000001, 0x8000000080008008
  ]

sanityCheckRoundConstants :: Bool
sanityCheckRoundConstants = (roundConstants24 == roundConstants24_ref)

--------------------------------------------------------------------------------

type State = Array Int Word64

index :: Int -> Int -> Int
index x y = x + y*5

--------------------------------------------------------------------------------

theta :: State -> State
theta old = new where

  cs :: Array Int Word64
  cs = listArray (0,4) 
    [ xors [ old!(index x y) | y <-[0..4] ] 
    | x <- [0..4] 
    ] 

  new = array (0,24) 
    [ ( index x y , (old!(index x y)) `xor` a `xor` b )
    | x<-[0..4]
    , y<-[0..4] 
    , let a =          cs ! (mod (x-1) 5)
    , let b = rotateL (cs ! (mod (x+1) 5)) 1
    ]

--------------------------------------------------------------------------------

rho :: State -> State
rho old = new where
  new = array (0,24) 
    [ ( index x y , rotateL (old!(index x y)) (rotationOffsets!(x,y)) ) 
    | x<-[0..4]
    , y<-[0..4] 
    ]

rotationOffsets :: Array (Int,Int) Int
rotationOffsets = array ((0,0),(4,4)) list where
  list = ((0,0),0) : go 0 (1,0)
  go 24 _     = []
  go t  (x,y) = ((x,y),ofs):rest where
    ofs0 = div ((t+1)*(t+2)) 2
    ofs  = mod ofs0 64
    rest = go (t+1) (x',y')
    x'   = y
    y'   = mod (2*x+3*y) 5

--------------------------------------------------------------------------------

pi_ :: State -> State
pi_ old = new where
  new = array (0,24) 
    [ ( index x y , old!(index (mod (x+3*y) 5) x) )
    | x<-[0..4]
    , y<-[0..4] 
    ]

--------------------------------------------------------------------------------

chi :: State -> State
chi old = new where
  new = array (0,24) 
    [ ( index x y , a `xor` ((complement b) .&. c) )
    | x<-[0..4]
    , y<-[0..4] 
    , let a = old!(index       x       y)
    , let b = old!(index (mod (x+1) 5) y)
    , let c = old!(index (mod (x+2) 5) y)
    ]

--------------------------------------------------------------------------------

iota' :: Word64 -> State -> State
iota' rc old = old // [ (0, (old!0) `xor` rc) ] 

iota :: Int -> State -> State
iota round_idx = iota' (roundConstants24 ! round_idx)

--------------------------------------------------------------------------------

step :: Int -> State -> State
step round_idx 
  = iota round_idx
  . chi
  . pi_
  . rho
  . theta

--------------------------------------------------------------------------------
-- * Keccak-p[1600,nr]

keccakP :: Int -> State -> State
keccakP nrounds = foldr (.) id (map step $ reverse [0..nrounds-1])

keccakF :: State -> State
keccakF = keccakP 24

--------------------------------------------------------------------------------
