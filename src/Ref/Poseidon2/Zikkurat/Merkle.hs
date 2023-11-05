
-- | Merkle tree built from Poseidon2 permutation

{-# LANGUAGE BangPatterns #-}
module Ref.Poseidon2.Zikkurat.Merkle where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits

import ZK.Algebra.Curves.BN128.Fr.Mont (Fr)

import Ref.Poseidon2.Zikkurat.Permutation

--------------------------------------------------------------------------------

calcMerkleRoot :: [Fr] -> Fr
calcMerkleRoot = go where
  go []  = error "calcMerkleRoot: input is empty"
  go [x] = x
  go xs  = go (map compressPair $ pairs xs)

compressPair :: (Fr,Fr) -> Fr 
compressPair (x,y) = compression x y

compression :: Fr -> Fr -> Fr 
compression x y = case permutation (x,y,0) of (z,_,_) -> z

pairs :: [Fr] -> [(Fr,Fr)]
pairs []         = []
pairs [x]        = (x,x) : []
pairs (x:y:rest) = (x,y) : pairs rest

--------------------------------------------------------------------------------

printExampleMerkleRoots :: IO ()
printExampleMerkleRoots = do
  putStrLn $ "Merkle root for [1..   1] = " ++ show (calcMerkleRoot $ map fromInteger [1..   1])
  putStrLn $ "Merkle root for [1..   2] = " ++ show (calcMerkleRoot $ map fromInteger [1..   2])
  putStrLn $ "Merkle root for [1..   4] = " ++ show (calcMerkleRoot $ map fromInteger [1..   4])
  putStrLn $ "Merkle root for [1..  16] = " ++ show (calcMerkleRoot $ map fromInteger [1..  16])
  putStrLn $ "Merkle root for [1..  64] = " ++ show (calcMerkleRoot $ map fromInteger [1..  64])
  putStrLn $ "Merkle root for [1.. 256] = " ++ show (calcMerkleRoot $ map fromInteger [1.. 256])
  putStrLn $ "Merkle root for [1..1024] = " ++ show (calcMerkleRoot $ map fromInteger [1..1024])

--------------------------------------------------------------------------------
