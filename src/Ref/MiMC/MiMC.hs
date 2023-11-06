
-- | MiMC-p/p and MiMC-2p/p-Feistel block ciphers, permutations and hash functions
--
-- Reference implementation for testing purposes.
--
-- Based on <https://eprint.iacr.org/2016/492>
--

{-# LANGUAGE BangPatterns #-}
module Ref.MiMC.MiMC where

--------------------------------------------------------------------------------

import Prelude hiding (exponent)

import Data.List
import Data.Array

import Control.Monad

import ZK.Algebra.Class.Field
import ZK.Algebra.Curves.BN128.Fr.Mont

import qualified Ref.MiMC.RoundConst as RC

--------------------------------------------------------------------------------

nrounds :: Int
nrounds = 110

exponent :: Integer
exponent = 5

-- | @(a^5)^invExponent == a@ (modulo prime)
invExponent :: Integer
invExponent = 17510594297471420177797124596205820070838691520332827474958563349260646796493

roundConsts :: Array Int Fr
roundConsts = listArray (0,2*nrounds-1) $ map fromInteger RC.roundConsts

type Key = Fr

--------------------------------------------------------------------------------

frToInteger :: Fr -> Integer
frToInteger = ZK.Algebra.Curves.BN128.Fr.Mont.from

arrayLength :: Array Int a -> Int
arrayLength arr = b - a + 1 where
  (a,b) = bounds arr

--------------------------------------------------------------------------------
-- * MiMC p/p

-- | MiMC p/p block cipher, encrypt with key
mimcEncryptBlock :: Key -> Fr -> Fr
mimcEncryptBlock key input = final + key where
  final = foldl' roundfun input [0..nrounds-1]
  roundfun :: Fr -> Int -> Fr
  roundfun !x !i = power (x + key + roundConsts!i) exponent

-- | MiMC p/p block cipher, decrypt with key
mimcDecryptBlock :: Key -> Fr -> Fr
mimcDecryptBlock key input = final where
  final = foldl' roundfun (input - key) (reverse [0..nrounds-1])
  roundfun :: Fr -> Int -> Fr
  roundfun !x !i = power x invExponent - key - roundConsts!i

-- | MiMC p/p permutation
mimcPermute :: Fr -> Fr
mimcPermute = mimcEncryptBlock 0

-- | MiMC p/p inverse permutation
mimcInvPermute :: Fr -> Fr
mimcInvPermute = mimcDecryptBlock 0

-- | Davies-Meyer compression
mimcCompress :: Fr -> Fr -> Fr
mimcCompress !k !x = mimcEncryptBlock k x + x

-- | MiMC p/p hash using the Merkle-Damgard construction
--
-- Note: we use the length of the input as the initialization vector,
-- for extra safety \/ domain separation. This matches the circom implementation
-- (there the length is statically known anyway).
--
mimcHashMerkleDamgard :: Array Int Fr -> Fr
mimcHashMerkleDamgard arr = hash where
  len  = arrayLength arr
  hash = foldl' mimcCompress (fromIntegral len) (elems arr)

mimcHashMerkleDamgardList :: [Fr] -> Fr
mimcHashMerkleDamgardList list 
  = mimcHashMerkleDamgard 
  $ listArray (1,length list) list

--------------------------------------------------------------------------------
-- * MiMC p/p stream cipher

type IV = Fr

-- | Encrypt a stream using CFB (Cipher feedback) mode.
--
-- Note: The IV should be ideally random and not repeating.
--
mimcEncryptCFB :: Key -> IV -> [Fr] -> [Fr] 
mimcEncryptCFB key iv xs = snd (mapAccumL f iv xs) where
  f !old !x = let y = mimcEncryptBlock key old + x in (y,y)

-- | Decrypt a stream encoded using CFB (Cipher feedback) mode.
mimcDecryptCFB :: Key -> IV -> [Fr] -> [Fr]
mimcDecryptCFB key iv ys = snd (mapAccumL f iv ys) where
  f !old !y = let x = y - mimcEncryptBlock key old in (y,x)

{-
key = 123456   :: Fr
iv  = 78901234 :: Fr
xs  = map fromInteger [1..10] :: [Fr]
ys  = mimcEncryptCFB key iv xs
zs  = mimcDecryptCFB key iv ys
-}

--------------------------------------------------------------------------------
-- * MiMC 2p/p (MiMC-Feistel)

-- | MiMC-Feistel 2p/p block cipher, encrypt with key
mimcFeistelEncryptBlock :: Key -> (Fr,Fr) -> (Fr,Fr)
mimcFeistelEncryptBlock key input = (fx+key, fy) where
  (fx,fy) = foldl' roundfun input [0..2*nrounds-1]
  roundfun :: (Fr,Fr) -> Int -> (Fr,Fr)
  roundfun (!l,!r) !i = (l',r') where
    l' = r + power (l + key + roundConsts!i) exponent
    r' = l 

-- | MiMC-Feistel 2p/p block cipher, decrypt with key
mimcFeistelDecryptBlock :: Key -> (Fr,Fr) -> (Fr,Fr)
mimcFeistelDecryptBlock key (x0,y0) = final where
  final = foldl' roundfun (x0-key,y0) (reverse [0..2*nrounds-1])
  roundfun :: (Fr,Fr) -> Int -> (Fr,Fr)
  roundfun (!l',!r') !i = (l,r) where
    l = r' 
    r = l' - power (l + key + roundConsts!i) exponent

-- | MiMC-Feistel 2p/p permutation
mimcFeistelPermute :: (Fr,Fr) -> (Fr,Fr)
mimcFeistelPermute = mimcFeistelEncryptBlock 0

-- | MiMC-Feistel 2p/p inverse permutation
mimcFeistelInvPermute :: (Fr,Fr) -> (Fr,Fr)
mimcFeistelInvPermute = mimcFeistelDecryptBlock 0

-- | MiMC-Feistel 2p/p hash, using the sponge construction with @rate = capacity = 1@.
--
-- Note: we use the length of the input to initialize the capacity,
-- for extra safety \/ domain separation. This matches the circom implementation
-- (there the length is statically known anyway).
--
mimcFeistelHashSponge :: Array Int Fr -> Fr
mimcFeistelHashSponge arr = fst final where
  n     = arrayLength arr
  ini   = (0,fromIntegral n) :: (Fr,Fr)
  final = foldl' step ini (elems arr)
  step :: (Fr,Fr) -> Fr -> (Fr,Fr)
  step (s0,s1) x = mimcFeistelPermute (s0+x, s1)

mimcFeistelHashSpongeList :: [Fr] -> Fr
mimcFeistelHashSpongeList list 
  = mimcFeistelHashSponge
  $ listArray (1,length list) list

--------------------------------------------------------------------------------

{-
k = 12974129847126457322013981412084940789733143757098366213678646546648721642 :: Fr
x = 50978548245213428109742832592359357938234235235235235753496498793573894531 :: Fr
y = 30896769435421324346587594958653952008778647876072132102910485679291292725 :: Fr
z = 98143125261234463780493858486821803099315348756201957438240478528954835923 :: Fr
-}

--------------------------------------------------------------------------------

-- | Sanity checks (encrypt then decrypt is the identity, etc)
mimcSanityTests :: Int -> IO ()
mimcSanityTests n = do

  generic "MiMC p/p" "decrypt(encrypt(x)) == x" rnd_kx print_kx $ 
    \(k,x) -> let z = mimcDecryptBlock k (mimcEncryptBlock k x) in (x==z)

  generic "MiMC p/p" "encrypt(decrypt(x)) == x" rnd_kx print_kx $ 
    \(k,x) -> let z = mimcEncryptBlock k (mimcDecryptBlock k x) in (x==z)

  generic "MiMC p/p" "invpermute(permute(x)) == x" rnd_x print_x $ 
    \x -> let z = mimcInvPermute (mimcPermute x) in (x==z)

  generic "MiMC p/p" "permute(invpermute(x)) == x" rnd_x print_x $ 
    \x -> let z = mimcPermute (mimcInvPermute x) in (x==z)

  generic "MiMC p/p CFB stream cipher" "decrypt(encrypt(xs)) == xs" rnd_ki_xs print_ki_xs $ 
    \(key,iv,xs) -> let zs = mimcDecryptCFB key iv (mimcEncryptCFB key iv xs) in (xs==zs)

  generic "MiMC-Feistel 2p/p" "decrypt(encrypt(x,y)) == (x,y)" rnd_kxy print_kxy $ 
    \(k,xy) -> let zw = mimcFeistelDecryptBlock k (mimcFeistelEncryptBlock k xy) in (xy==zw)

  generic "MiMC-Feistel 2p/p" "encrypt(decrypt(x,y)) == (x,y)" rnd_kxy print_kxy $ 
    \(k,xy) -> let zw = mimcFeistelEncryptBlock k (mimcFeistelDecryptBlock k xy) in (xy==zw)

  generic "MiMC-Feistel 2p/p" "invpermute(permute(x,y)) == (x,y)" rnd_xy print_xy $ 
    \xy -> let zw = mimcFeistelInvPermute (mimcFeistelPermute xy) in (xy==zw)

  generic "MiMC-Feistel 2p/p" "permute(invpermute(x,y)) == (x,y)" rnd_xy print_xy $ 
    \xy -> let zw = mimcFeistelPermute (mimcFeistelInvPermute xy) in (xy==zw)

  where

    generic which what rnd printIt testIt = do
      putStrLn $ ""
      putStrLn $ which
      putStrLn $ what
      putStrLn $ "doing " ++ show n ++ " random tests"
      forM_ [1..n] $ \i -> do
        xy <- rnd
        let b = testIt xy
        unless b $ do
          putStrLn $ "test #" ++ show i ++ " FAILED!"
          printIt xy
    
    rnd_x = do
      x <- rndIO :: IO Fr
      return x

    rnd_xy = do
      x <- rndIO :: IO Fr
      y <- rndIO :: IO Fr
      return (x,y)

    rnd_kx = do
      k <- rndIO :: IO Fr
      x <- rndIO :: IO Fr
      return (k,x)

    rnd_kxy = do
      k <- rndIO :: IO Fr
      x <- rndIO :: IO Fr
      y <- rndIO :: IO Fr
      return (k,(x,y))

    rnd_ki_xs = do
      k  <- rndIO :: IO Fr
      iv <- rndIO :: IO Fr
      xs <- replicateM 10 rndIO :: IO [Fr]
      return (k,iv,xs)

    print_x x = do
      putStrLn $ "  x = " ++ show x

    print_xy (x,y) = do
      putStrLn $ "  x = " ++ show x
      putStrLn $ "  y = " ++ show y

    print_kx (k,x) = do
      putStrLn $ "  k = " ++ show k
      putStrLn $ "  x = " ++ show x

    print_kxy (k,(x,y)) = do
      putStrLn $ "  k = " ++ show k
      putStrLn $ "  x = " ++ show x
      putStrLn $ "  y = " ++ show y

    print_ki_xs (k,iv,xs) = do
      putStrLn $ "  key = " ++ show k
      putStrLn $ "  iv  = " ++ show iv
      putStrLn $ "  xs  = " ++ show xs

--------------------------------------------------------------------------------
