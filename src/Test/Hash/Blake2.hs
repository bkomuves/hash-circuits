
module Test.Hash.Blake2 where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner
import Vectors.Blake2

--------------------------------------------------------------------------------

blake2s_256_test :: SimpleHashTest
blake2s_256_test = SimpleHashTest
  { __circomFile     = "circuits/blake2/blake2s.circom"
  , __templateName   = "Blake2s_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = blake2s_256_vectors
  }

blake2b_256_test :: SimpleHashTest
blake2b_256_test = SimpleHashTest
  { __circomFile     = "circuits/blake2/blake2b.circom"
  , __templateName   = "Blake2b_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = blake2b_256_vectors
  }

--------------------------------------------------------------------------------

runTests_BLAKE2 :: Verbosity -> FilePath -> IO ()
runTests_BLAKE2 verbosity rootDir = do

  putStrLn "running test for BLAKE2..."

  runSimpleTestBytes verbosity rootDir blake2s_256_test
  runSimpleTestBytes verbosity rootDir blake2b_256_test

--------------------------------------------------------------------------------

