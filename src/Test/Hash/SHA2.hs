
module Test.Hash.SHA2 where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner
import Vectors.SHA2

--------------------------------------------------------------------------------

sha256_test :: SimpleHashTest
sha256_test = SimpleHashTest
  { __circomFile     = "circuits/sha256/sha256_hash_bytes.circom"
  , __templateName   = "Sha256_hash_bytes"
  , __inputSignal    = "bytes"
  , __outputSignal   = "out_hash_bytes"
  , __testCases      = sha2_256_vectors
  }

sha512_test :: SimpleHashTest
sha512_test = SimpleHashTest
  { __circomFile     = "circuits/sha512/sha512_hash_bytes.circom"
  , __templateName   = "Sha512_hash_bytes_digest"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_512_vectors
  }

--------------------------------------------------------------------------------

runTests_SHA2 :: Verbosity -> FilePath -> IO ()
runTests_SHA2 verbosity rootDir = do

  putStrLn "running test for SHA2..."

  runSimpleTest verbosity rootDir sha512_test
  runSimpleTest verbosity rootDir sha256_test
