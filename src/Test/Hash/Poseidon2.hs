
module Test.Hash.Poseidon2 where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner

import Ref.Poseidon2.Zikkurat.Permutation
import Ref.Poseidon2.Zikkurat.Merkle
import Ref.Poseidon2.Zikkurat.Sponge

--------------------------------------------------------------------------------
-- * tests for the hashes where the input is a sequence of field elements

poseidon2_sponge_rate1_test :: ArithHashTest
poseidon2_sponge_rate1_test = MkGenericHashTest
  { __circomFile     = "circuits/poseidon2/poseidon2_sponge.circom"
  , __templateName   = "Poseidon2_sponge_hash_rate_1"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = poseidon2_sponge_rate1_vectors
  }

poseidon2_sponge_rate2_test :: ArithHashTest
poseidon2_sponge_rate2_test = MkGenericHashTest
  { __circomFile     = "circuits/poseidon2/poseidon2_sponge.circom"
  , __templateName   = "Poseidon2_sponge_hash_rate_2"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = poseidon2_sponge_rate2_vectors
  }

poseidon2_merkle_tree_test :: ArithHashTest
poseidon2_merkle_tree_test = MkGenericHashTest
  { __circomFile     = "circuits/poseidon2/poseidon2_tests.circom"
  , __templateName   = "Test_Poseidon2_merkle_tree"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = poseidon2_merkle_tree_vectors
  }

--------------------------------------------------------------------------------

poseidon2_sponge_rate1_vectors :: [([Integer], Integer)]
poseidon2_sponge_rate1_vectors = 
  [ (inp, frToInteger hash)
  | k<-[0..10]
  , let inp  = [1..k] 
  , let hash = spongeRate1 (map fromInteger inp) 
  ]

poseidon2_sponge_rate2_vectors :: [([Integer], Integer)]
poseidon2_sponge_rate2_vectors = 
  [ (inp, frToInteger hash)
  | k<-[0..10]
  , let inp  = [1..k] 
  , let hash = spongeRate2 (map fromInteger inp) 
  ]

poseidon2_merkle_tree_vectors :: [([Integer], Integer)]
poseidon2_merkle_tree_vectors = 
  [ (inp, frToInteger hash)
  | k<-[0..4]
  , let inp  = [1..2^k] 
  , let hash = calcMerkleRoot (map fromInteger inp) 
  ]

--------------------------------------------------------------------------------
-- * tests for the permutation

poseidon2_perm_test :: ArithHashTest
poseidon2_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/poseidon2/poseidon2_tests.circom"
  , __templateName   = "Test_Poseidon2_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = poseidon2_perm_vectors
  }

poseidon2_perm_vectors :: [([Integer], Integer)]
poseidon2_perm_vectors = 
  [ ([x,y,z], frToInteger t)
  | i<-[0..10]
  , let x = 101   + 2*i
  , let y = 2002  - 3*i
  , let z = 30003 - 7*i
  , let (u,v,w) = permutation (fromInteger x, fromInteger y, fromInteger z)
  , let t = u + v + w
  ]

--------------------------------------------------------------------------------
-- * tests for the remaining components

poseidon2_compression_test :: ArithHashTest
poseidon2_compression_test = MkGenericHashTest
  { __circomFile     = "circuits/poseidon2/poseidon2_tests.circom"
  , __templateName   = "Test_Poseidon2_compression"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = poseidon2_compression_vectors
  }

poseidon2_compression_vectors :: [([Integer], Integer)]
poseidon2_compression_vectors = 
  [ ([x,y], frToInteger z)
  | i<-[0..10]
  , let x =  555 +   i
  , let y = 1001 + 2*i
  , let z = compression (fromInteger x) (fromInteger y)
  ]

--------------------------------------------------------------------------------

runTests_Poseidon2 :: Verbosity -> FilePath -> IO ()
runTests_Poseidon2 verbosity rootDir = do

  putStrLn "running tests for Poseidon2... (input = sequence of field elements)"

  runArithTest verbosity rootDir poseidon2_sponge_rate1_test
  runArithTest verbosity rootDir poseidon2_sponge_rate2_test
  runArithTest verbosity rootDir poseidon2_merkle_tree_test

  runArithTest verbosity rootDir poseidon2_compression_test
  runArithTest verbosity rootDir poseidon2_perm_test

--------------------------------------------------------------------------------
