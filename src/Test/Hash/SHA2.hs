
module Test.Hash.SHA2 where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner
import Vectors.SHA2

--------------------------------------------------------------------------------
-- * tests for the version where the input is a sequence of bytes

sha224_test :: SimpleHashTest
sha224_test = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha224/sha224_hash_bytes.circom"
  , __templateName   = "Sha224_hash_bytes_digest"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_224_vectors
  }

sha256_test :: SimpleHashTest
sha256_test = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha256/sha256_hash_bytes.circom"
  , __templateName   = "Sha256_hash_bytes_digest"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_256_vectors
  }

sha384_test :: SimpleHashTest
sha384_test = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha384/sha384_hash_bytes.circom"
  , __templateName   = "Sha384_hash_bytes_digest"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_384_vectors
  }

sha512_test :: SimpleHashTest
sha512_test = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha512/sha512_hash_bytes.circom"
  , __templateName   = "Sha512_hash_bytes_digest"
  , __inputSignal    = "inp_bytes"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_512_vectors
  }

----------------------------------------

runTests_SHA2_bytes :: Verbosity -> FilePath -> IO ()
runTests_SHA2_bytes verbosity rootDir = do

  putStrLn "running test for SHA2... (input = byte sequence versions)"

  runSimpleTestBytes verbosity rootDir sha224_test
  runSimpleTestBytes verbosity rootDir sha256_test
  runSimpleTestBytes verbosity rootDir sha384_test
  runSimpleTestBytes verbosity rootDir sha512_test

--------------------------------------------------------------------------------
-- * tests for the version where the input is a sequence of bits

sha224_test_bits :: SimpleHashTest
sha224_test_bits = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha224/sha224_hash_bits.circom"
  , __templateName   = "Sha224_hash_bits_digest"
  , __inputSignal    = "inp_bits"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_224_vectors
  }

sha256_test_bits :: SimpleHashTest
sha256_test_bits = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha256/sha256_hash_bits.circom"
  , __templateName   = "Sha256_hash_bits_digest"
  , __inputSignal    = "inp_bits"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_256_vectors
  }

sha384_test_bits :: SimpleHashTest
sha384_test_bits = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha384/sha384_hash_bits.circom"
  , __templateName   = "Sha384_hash_bits_digest"
  , __inputSignal    = "inp_bits"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_384_vectors
  }

sha512_test_bits :: SimpleHashTest
sha512_test_bits = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha512/sha512_hash_bits.circom"
  , __templateName   = "Sha512_hash_bits_digest"
  , __inputSignal    = "inp_bits"
  , __outputSignal   = "hash_bytes"
  , __testCases      = sha2_512_vectors
  }

----------------------------------------

runTests_SHA2_bits :: Verbosity -> FilePath -> IO ()
runTests_SHA2_bits verbosity rootDir = do

  putStrLn "running test for SHA2... (input = bit sequence versions)"

  runSimpleTestBits verbosity rootDir sha224_test_bits
  runSimpleTestBits verbosity rootDir sha256_test_bits
  runSimpleTestBits verbosity rootDir sha384_test_bits
  runSimpleTestBits verbosity rootDir sha512_test_bits

--------------------------------------------------------------------------------
-- * tests for the versions where the input is a single chunk

sha256_test_chunk :: SimpleHashTest
sha256_test_chunk = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha256/sha256_hash_chunk.circom"
  , __templateName   = "Sha256_hash_chunk"
  , __inputSignal    = "inp_bits"
  , __outputSignal   = "out_bits"
  , __testCases      = sha2_256_vectors
  }

sha512_test_chunk :: SimpleHashTest
sha512_test_chunk = MkGenericHashTest
  { __circomFile     = "circuits/sha2/sha512/sha512_hash_chunk.circom"
  , __templateName   = "Sha512_hash_chunk"
  , __inputSignal    = "inp_bits"
  , __outputSignal   = "out_bits"
  , __testCases      = sha2_512_vectors
  }

----------------------------------------

runTests_SHA2_chunk :: Verbosity -> FilePath -> IO ()
runTests_SHA2_chunk verbosity rootDir = do

  putStrLn "running test for SHA2... (input = single chunk version)"

  runChunkTest_SHA256 verbosity rootDir sha256_test_chunk
  runChunkTest_SHA512 verbosity rootDir sha512_test_chunk

--------------------------------------------------------------------------------
