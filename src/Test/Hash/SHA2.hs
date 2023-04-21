
module Test.Hash.SHA2 where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner
import Vectors.SHA2

--------------------------------------------------------------------------------

sha256_test :: SimpleHashTest
sha256_test = SimpleHashTest
  { __circomFile     = "circuits/sha256/sha_hash_bytes.circom"
  , __templateName   = "Sha256_hash_bytes"
  , __inputSignal    = "bytes"
  , __outputSignal   = "out_hash_bytes"
  , __testCases      = sha2_256_vectors
  }

--------------------------------------------------------------------------------

runTests_SHA2 :: Verbosity -> FilePath -> IO ()
runTests_SHA2 verbosity rootDir = do

  putStrLn "running test for SHA2..."

  runSimpleTest verbosity rootDir sha256_test