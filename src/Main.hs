
module Main where

--------------------------------------------------------------------------------

import Data.Char
import System.Environment

import R1CS.Misc ( Verbosity(..) )

import qualified Test.Hash.SHA2      as SHA2
import qualified Test.Hash.Keccak    as Keccak
import qualified Test.Hash.Blake2    as Blake2
import qualified Test.Hash.MiMC      as MiMC
import qualified Test.Hash.Poseidon2 as Poseidon2

--------------------------------------------------------------------------------

help = do
  putStrLn ""
  putStrLn "usage:"
  putStrLn ""
  putStrLn "$ hash-circuit-tests <testsuite>"
  putStrLn ""
  putStrLn "where <testsuite> is one of the following:"
  putStrLn ""
  putStrLn " - sha2         (input = bytes)"
  putStrLn " - sha2-bits    (input = bits)"
  putStrLn " - sha2-chunk   (input = single chunk)"
  putStrLn " - keccak       (original Keccak variant)"
  putStrLn " - sha3         (NIST Keccak variant)"
  putStrLn " - shake        (NIST SHA3 XOFs)"
  putStrLn " - blake2"
  putStrLn " - poseidon2"
  putStrLn " - mimc         (MiMC and MiMC-Feistel hashes)"
  putStrLn " - mimc-comp    (various MiMC components)"
  putStrLn " - mimc-stream  (MiMC stream ciphers)"
  putStrLn ""

main = do

  let verbosity = Silent
  let rootDir   = "."

  args <- getArgs 
  case args of

    [what] -> case map toLower what of

      "sha2"           -> SHA2.runTests_SHA2_bytes      verbosity rootDir
      "sha2-bits"      -> SHA2.runTests_SHA2_bits       verbosity rootDir
      "sha2-chunk"     -> SHA2.runTests_SHA2_chunk      verbosity rootDir
      "keccak"         -> Keccak.runTests_Keccak        verbosity rootDir
      "sha3"           -> Keccak.runTests_SHA3          verbosity rootDir
      "shake"          -> Keccak.runTests_SHAKE         verbosity rootDir
      "blake2"         -> Blake2.runTests_BLAKE2        verbosity rootDir
      "poseidon2"      -> Poseidon2.runTests_Poseidon2  verbosity rootDir
      "mimc"           -> MiMC.runTests_MiMC            verbosity rootDir
      "mimc-comp"      -> MiMC.runTests_MiMC_components verbosity rootDir
      "mimc-stream"    -> MiMC.runTests_MiMC_stream     verbosity rootDir

      _ -> do
        putStrLn $ "unknown testsuite `" ++ what ++ "`"
        help
 
    _ -> help
