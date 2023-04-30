
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

keccak_224_test :: SimpleHashTest
keccak_224_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/keccak_bytes.circom"
  , __templateName   = "Keccak_224_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = keccak_224_vectors
  }

keccak_256_test :: SimpleHashTest
keccak_256_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/keccak_bytes.circom"
  , __templateName   = "Keccak_256_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = keccak_256_vectors
  }

keccak_384_test :: SimpleHashTest
keccak_384_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/keccak_bytes.circom"
  , __templateName   = "Keccak_384_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = keccak_384_vectors
  }

keccak_512_test :: SimpleHashTest
keccak_512_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/keccak_bytes.circom"
  , __templateName   = "Keccak_512_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = keccak_512_vectors
  }

--------------------------------------------------------------------------------

shake_128_test :: SimpleHashTest
shake_128_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/sha3_bytes.circom"
  , __templateName   = "SHAKE128_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = shake_128_5000
  }

shake_256_test :: SimpleHashTest
shake_256_test = SimpleHashTest
  { __circomFile     = "circuits/keccak/sha3_bytes.circom"
  , __templateName   = "SHAKE256_bytes"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "out_bytes"
  , __testCases      = shake_256_5000
  }

--------------------------------------------------------------------------------

runTests_SHA3 :: Verbosity -> FilePath -> IO ()
runTests_SHA3 verbosity rootDir = do

  putStrLn "running test for SHA3..."

  runSimpleTestBytes verbosity rootDir sha3_224_test
  runSimpleTestBytes verbosity rootDir sha3_256_test
  runSimpleTestBytes verbosity rootDir sha3_384_test
  runSimpleTestBytes verbosity rootDir sha3_512_test

--------------------------------------------------------------------------------

runTests_Keccak :: Verbosity -> FilePath -> IO ()
runTests_Keccak verbosity rootDir = do

  putStrLn "running test for Keccak..."

  runSimpleTestBytes verbosity rootDir keccak_224_test
  runSimpleTestBytes verbosity rootDir keccak_256_test
  runSimpleTestBytes verbosity rootDir keccak_384_test
  runSimpleTestBytes verbosity rootDir keccak_512_test

--------------------------------------------------------------------------------

runTests_SHAKE :: Verbosity -> FilePath -> IO ()
runTests_SHAKE verbosity rootDir = do

  putStrLn "running test for SHAKE..."

  runShakeTest verbosity rootDir shake_128_test
  runShakeTest verbosity rootDir shake_256_test

--------------------------------------------------------------------------------
