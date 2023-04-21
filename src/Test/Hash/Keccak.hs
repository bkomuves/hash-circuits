
module Test.Hash.Keccak where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner
import Vectors.Keccak

--------------------------------------------------------------------------------

sha3_224_test :: SimpleHashTest
sha3_224_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/sha3_bytes.circom"
  , __templateName   = "SHA3_224_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = sha3_224_vectors
  }

sha3_256_test :: SimpleHashTest
sha3_256_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/sha3_bytes.circom"
  , __templateName   = "SHA3_256_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = sha3_256_vectors
  }

sha3_384_test :: SimpleHashTest
sha3_384_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/sha3_bytes.circom"
  , __templateName   = "SHA3_384_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = sha3_384_vectors
  }

sha3_512_test :: SimpleHashTest
sha3_512_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/sha3_bytes.circom"
  , __templateName   = "SHA3_512_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = sha3_512_vectors
  }

--------------------------------------------------------------------------------

runTests_SHA3 :: Verbosity -> FilePath -> IO ()
runTests_SHA3 verbosity rootDir = do

  putStrLn "running test for SHA3..."

  runSimpleTest verbosity rootDir sha3_224_test
  runSimpleTest verbosity rootDir sha3_256_test
  runSimpleTest verbosity rootDir sha3_384_test
  runSimpleTest verbosity rootDir sha3_512_test

--------------------------------------------------------------------------------
