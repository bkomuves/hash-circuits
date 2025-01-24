
module Test.Hash.Griffin where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner

import Ref.Griffin.Permutation

--------------------------------------------------------------------------------
-- * tests for the permutation

griffin_perm_test :: ArithHashTest
griffin_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/griffin/griffin_tests.circom"
  , __templateName   = "Test_Griffin_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = griffin_perm_vectors
  }

griffin_perm_vectors :: [([Integer], Integer)]
griffin_perm_vectors = 
  [ ([x,y,z], frToInteger t)
  | i<-[0..10]
  , let x = 101   + 2*i
  , let y = 2002  - 3*i
  , let z = 30003 - 7*i
  , let (u,v,w) = permute (fromInteger x, fromInteger y, fromInteger z)
  , let t = u + v + w
  ]

--------------------------------------------------------------------------------

griffin_iter_perm_test :: ArithHashTest
griffin_iter_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/griffin/griffin_tests.circom"
  , __templateName   = "Test_Griffin_iterated_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = griffin_iter_perm_vectors
  }

griffin_iter_perm_vectors :: [([Integer], Integer)]
griffin_iter_perm_vectors = 
  [ ([x,y,z], frToInteger t)
  | i<-[0..10]
  , let x = 505 + 2*i
  , let y = 606 - 3*i
  , let z = 707 - 7*i
  , let input = (fromInteger x, fromInteger y, fromInteger z)
  , let (u,v,w) = iter 100 permute input
  , let t = u + v + w
  ]

--------------------------------------------------------------------------------
-- * tests for the remaining components

griffin_compression_test :: ArithHashTest
griffin_compression_test = MkGenericHashTest
  { __circomFile     = "circuits/griffin/griffin_tests.circom"
  , __templateName   = "Test_Griffin_compression"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = griffin_compression_vectors
  }

griffin_compression_vectors :: [([Integer], Integer)]
griffin_compression_vectors = 
  [ ([x,y], frToInteger z)
  | i<-[0..10]
  , let x =  555 +   i
  , let y = 1001 + 2*i
  , let z = compress (fromInteger x) (fromInteger y)
  ]

--------------------------------------------------------------------------------

runTests_Griffin :: Verbosity -> FilePath -> IO ()
runTests_Griffin verbosity rootDir = do

  putStrLn "running tests for Griffin..."

  runArithTest verbosity rootDir griffin_perm_test
  runArithTest verbosity rootDir griffin_compression_test

  putStrLn "running tests for 100x iterated Griffin..."

  runArithTest verbosity rootDir griffin_iter_perm_test

--------------------------------------------------------------------------------
