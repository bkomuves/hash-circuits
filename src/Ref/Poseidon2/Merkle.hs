
-- | Merkle tree built from Poseidon2 permutation

module Ref.Poseidon2.Merkle where

--------------------------------------------------------------------------------

import Ref.Poseidon2.Permutation
import Ref.Poseidon2.BN256

--------------------------------------------------------------------------------

merkleRoot :: [BN256] -> BN256
merkleRoot []  = error "merkleRoot: input is empty"
merkleRoot [x] = x
merkleRoot xs  = merkleRoot $ map compression $ pairs xs

compression :: (BN256,BN256) -> BN256 
compression (x,y) = case permutation (x,y,0) of (z,_,_) -> z

pairs :: [BN256] -> [(BN256,BN256)]
pairs []         = []
pairs [x]        = (x,x) : []
pairs (x:y:rest) = (x,y) : pairs rest

--------------------------------------------------------------------------------

printExampleMerkleRoots :: IO ()
printExampleMerkleRoots = do
  putStrLn $ "Merkle root for [1..   1] = " ++ show (merkleRoot $ map toBN256 [1..   1])
  putStrLn $ "Merkle root for [1..   2] = " ++ show (merkleRoot $ map toBN256 [1..   2])
  putStrLn $ "Merkle root for [1..   4] = " ++ show (merkleRoot $ map toBN256 [1..   4])
  putStrLn $ "Merkle root for [1..  16] = " ++ show (merkleRoot $ map toBN256 [1..  16])
  putStrLn $ "Merkle root for [1..  64] = " ++ show (merkleRoot $ map toBN256 [1..  64])
  putStrLn $ "Merkle root for [1.. 256] = " ++ show (merkleRoot $ map toBN256 [1.. 256])
  putStrLn $ "Merkle root for [1..1024] = " ++ show (merkleRoot $ map toBN256 [1..1024])
  -- putStrLn $ "merkle root for [1..4096] = " ++ show (merkleRoot $ map toBN256 [1..4096])

--------------------------------------------------------------------------------
