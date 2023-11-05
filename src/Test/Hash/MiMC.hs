
module Test.Hash.MiMC where

--------------------------------------------------------------------------------

import R1CS
import Test.Runner
import Ref.MiMC.MiMC

--------------------------------------------------------------------------------
-- * tests for the hashes where the input is a sequence of field elements

mimc_merkle_damgard_hash_test :: ArithHashTest
mimc_merkle_damgard_hash_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-p-p.circom"
  , __templateName   = "MiMC_p$p_hash_MerkleDamgard"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_MD_vectors
  }

mimc_feistel_sponge_hash_test :: ArithHashTest
mimc_feistel_sponge_hash_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-feistel-2p-p.circom"
  , __templateName   = "MiMC_Feistel_2p$p_hash_sponge"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_sponge_vectors
  }

--------------------------------------------------------------------------------

mimc_MD_vectors :: [([Integer], Integer)]
mimc_MD_vectors = 
  [ (inp, frToInteger hash)
  | k<-[0..10]
  , let inp  = [1..k] 
  , let hash = mimcHashMerkleDamgardList (map fromInteger inp) 
  ]

mimc_sponge_vectors :: [([Integer], Integer)]
mimc_sponge_vectors = 
  [ (inp, frToInteger hash)
  | k<-[0..10]
  , let inp  = [1..k] 
  , let hash = mimcFeistelHashSpongeList (map fromInteger inp) 
  ]

--------------------------------------------------------------------------------
-- * tests for the permutations

mimc_perm_test :: ArithHashTest
mimc_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_perm_vectors
  }

mimc_inv_perm_test :: ArithHashTest
mimc_inv_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_inverse_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_inv_perm_vectors
  }

mimc_feistel_perm_test :: ArithHashTest
mimc_feistel_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_Feistel_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_feistel_perm_vectors
  }

mimc_feistel_inv_perm_test :: ArithHashTest
mimc_feistel_inv_perm_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_Feistel_inverse_permutation"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_feistel_inv_perm_vectors
  }

--------------------------------------------------------------------------------

mimc_perm_vectors :: [([Integer], Integer)]
mimc_perm_vectors = 
  [ ([x], frToInteger z)
  | x<-[0..10]
  , let z = mimcPermute (fromInteger x) 
  ]

mimc_inv_perm_vectors :: [([Integer], Integer)]
mimc_inv_perm_vectors = 
  [ ([x], frToInteger z)
  | x<-[0..10]
  , let z = mimcInvPermute (fromInteger x) 
  ]

mimc_feistel_perm_vectors :: [([Integer], Integer)]
mimc_feistel_perm_vectors = 
  [ ([x,y], frToInteger z)
  | x<-[0..10]
  , let y = 1000+x
  , let (u,v) = mimcFeistelPermute (fromInteger x, fromInteger y)
  , let z = u+v 
  ]

mimc_feistel_inv_perm_vectors :: [([Integer], Integer)]
mimc_feistel_inv_perm_vectors = 
  [ ([x,y], frToInteger z)
  | x<-[0..10]
  , let y = 1000+x
  , let (u,v) = mimcFeistelInvPermute (fromInteger x, fromInteger y)
  , let z = u+v 
  ]

--------------------------------------------------------------------------------
-- * tests for the remaining components

mimc_compression_test :: ArithHashTest
mimc_compression_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_compression"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_compression_vectors
  }

mimc_encrypt_test :: ArithHashTest
mimc_encrypt_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_encrypt"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_encrypt_vectors
  }

mimc_decrypt_test :: ArithHashTest
mimc_decrypt_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_decrypt"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_decrypt_vectors
  }


mimc_feistel_encrypt_test :: ArithHashTest
mimc_feistel_encrypt_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_Feistel_encrypt"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_feistel_encrypt_vectors
  }

mimc_feistel_decrypt_test :: ArithHashTest
mimc_feistel_decrypt_test = MkGenericHashTest
  { __circomFile     = "circuits/mimc/mimc-test.circom"
  , __templateName   = "Test_MiMC_Feistel_decrypt"
  , __inputSignal    = "inp"
  , __outputSignal   = "out"
  , __testCases      = mimc_feistel_decrypt_vectors
  }

--------------------------------------------------------------------------------

mimc_compression_vectors :: [([Integer], Integer)]
mimc_compression_vectors = 
  [ ([x,y], frToInteger z)
  | x<-[0..10]
  , let y = 1000+x
  , let z = mimcCompress (fromInteger x) (fromInteger y)
  ]

mimc_encrypt_vectors :: [([Integer], Integer)]
mimc_encrypt_vectors = 
  [ ([k,x], frToInteger z)
  | i<-[0..10]
  , let k =  201+i
  , let x = 1000+i
  , let z = mimcEncryptBlock (fromInteger k) (fromInteger x)
  ]

mimc_decrypt_vectors :: [([Integer], Integer)]
mimc_decrypt_vectors = 
  [ ([k,x], frToInteger z)
  | i<-[0..10]
  , let k =  201+i
  , let x = 1000+i
  , let z = mimcDecryptBlock (fromInteger k) (fromInteger x)
  ]

mimc_feistel_encrypt_vectors :: [([Integer], Integer)]
mimc_feistel_encrypt_vectors = 
  [ ([k,x,y], frToInteger z)
  | i<-[0..10]
  , let k =  201+i
  , let x = 1000+i
  , let y = 5555+i
  , let (u,v) = mimcFeistelEncryptBlock (fromInteger k) (fromInteger x, fromInteger y)
  , let z = u+v
  ]

mimc_feistel_decrypt_vectors :: [([Integer], Integer)]
mimc_feistel_decrypt_vectors = 
  [ ([k,x,y], frToInteger z)
  | i<-[0..10]
  , let k =  201+i
  , let x = 1000+i
  , let y = 5555+i
  , let (u,v) = mimcFeistelDecryptBlock (fromInteger k) (fromInteger x, fromInteger y)
  , let z = u+v
  ]


--------------------------------------------------------------------------------

runTests_MiMC :: Verbosity -> FilePath -> IO ()
runTests_MiMC verbosity rootDir = do
  putStrLn "running test for MiMC... (input = sequence of field elements)"
  runArithTest verbosity rootDir mimc_merkle_damgard_hash_test
  runArithTest verbosity rootDir mimc_feistel_sponge_hash_test

runTests_MiMC_components :: Verbosity -> FilePath -> IO ()
runTests_MiMC_components verbosity rootDir = do

  putStrLn "running test for MiMC components..."

  runArithTest verbosity rootDir mimc_perm_test
  runArithTest verbosity rootDir mimc_inv_perm_test
  runArithTest verbosity rootDir mimc_feistel_perm_test
  runArithTest verbosity rootDir mimc_feistel_inv_perm_test

  runArithTest verbosity rootDir mimc_compression_test

  runArithTest verbosity rootDir mimc_encrypt_test
  runArithTest verbosity rootDir mimc_decrypt_test
  runArithTest verbosity rootDir mimc_feistel_encrypt_test
  runArithTest verbosity rootDir mimc_feistel_decrypt_test

--------------------------------------------------------------------------------
