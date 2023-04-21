
module Main where

--------------------------------------------------------------------------------

import Data.Char
import System.Environment

import R1CS.Misc ( Verbosity(..) )

import qualified Test.Hash.SHA2   as SHA2
import qualified Test.Hash.Keccak as Keccak

--------------------------------------------------------------------------------

help = do
  putStrLn ""
  putStrLn "usage:"
  putStrLn ""
  putStrLn "$ hash-circuit-tests <testsuite>"
  putStrLn ""
  putStrLn "where <testsuite> is one of the following:"
  putStrLn ""
  putStrLn " - sha2"
  putStrLn " - sha3   (NIST variant)"
  putStrLn " - keccak (original variant)"
  putStrLn " - poseidon2"
  putStrLn ""

main = do

  let verbosity = Silent
  let rootDir   = "."

  args <- getArgs 
  case args of

    [what] -> case map toLower what of

      "sha2"           -> SHA2.runTests_SHA2     verbosity rootDir
      "sha3"           -> Keccak.runTests_SHA3   verbosity rootDir
--      "keccak"         -> Keccak.runTests_Keccak verbosity rootDir
      "poseidon2"      -> putStrLn "poseidon2 testsuite not implemented yet"

      _ -> do
        putStrLn $ "unknown testsuite `" ++ what ++ "`"
        help
 
    _ -> help
