
module Test.Runner where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits
import Data.Char

import Control.Monad
import System.FilePath
import System.IO

import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as C

import R1CS
import qualified R1CS.Parser.WtnsJSON as W
import qualified R1CS.Parser.SymFile  as S

import Ref.Common
import Test.Misc

--------------------------------------------------------------------------------

extractScalar :: Witness Name a -> Name -> a
extractScalar (Witness (Mapping mapping)) var = case Map.lookup var mapping of
  Just y  -> y
  Nothing -> error $ "extractScalar: key `" ++ var ++ "` not found in the witness"

extractArray :: Witness Name a -> Name -> [a]
extractArray (Witness (Mapping mapping)) var = go 0 where
  go i = case Map.lookup (var ++ "[" ++ show i ++ "]") mapping of
    Just y  -> y : go (i+1) 
    Nothing -> if i>0 
      then [] 
      else error $ "extractArray: array `" ++ var ++ "[]` not found in the witness"

extractBits :: Witness Name Integer -> Name -> [Bit]
extractBits witness var = map integerToBit $ extractArray witness var

extractBytes :: Witness Name Integer -> Name -> [Word8]
extractBytes witness var = map f $ extractArray witness var where
  f k = if k >= 0 && k < 256 
    then fromIntegral k 
    else error "extractBytes: value out of range"


--------------------------------------------------------------------------------

loadWitness_ :: Verbosity -> CircuitFiles -> WitnessFiles -> IO (Witness Name Integer)
loadWitness_ verbosity circuitfiles witnessfiles = do
  sym0  <- S.parseSymFile  (_symFile circuitfiles)
  loadWitness' sym0 witnessfiles

--------------------------------------------------------------------------------

-- | We assume that the circuit takes a sequence of bytes as input and
-- and also produces a sequence of bytes as output. The template parameter
-- is assumed to be the length of the input.
data SimpleHashTest = SimpleHashTest
  { __circomFile     :: FilePath
  , __templateName   :: String
  , __inputSignal    :: String
  , __outputSignal   :: String
  , __testCases      :: [(String,String)]
  }
  deriving Show

--------------------------------------------------------------------------------

runSimpleTest :: Verbosity -> FilePath -> SimpleHashTest -> IO ()
runSimpleTest verbosity rootdir test = do

  putStrLn $ "\nrunning tests for template `" ++ __templateName test ++ "`"

  let circom   = rootdir </> __circomFile test

  forM_ (zip [1..] (__testCases test)) $ \(idx, (input_str, output_str)) -> do

    putStr $ "test case #" ++ show idx ++ ": "
    hFlush stdout
    
    let input_bytes = map ord input_str :: [Int]

    if null input_str

      then do
        putStrLn "test for empty input string is skipped because of a bug in circom"

      else do
        let maincomp = MainComponent 
              { _templateName   = __templateName test
              , _templateParams = [length input_bytes]
              , _publicInputs   = [__inputSignal test]
              }
      
        circuitfiles <- compileCircomCircuit verbosity circom (Just maincomp)
        
        let hsinputs =  HsInputs $ Map.fromList
              [ (__inputSignal test , HsInput input_bytes)
              ] 
    
        witnessfiles <- computeWitness verbosity circuitfiles hsinputs
        wtns <- loadWitness_ verbosity circuitfiles witnessfiles
    
        let result   = extractBytes wtns ("main." ++ __outputSignal test)
        let hash_str = concatMap toHexString result
    
        if hash_str == output_str
          then putStrLn "OK."
          else do
            putStrLn "FAILED!"
            putStrLn $ " - expected = " ++ show output_str
            putStrLn $ " - actual   = " ++ show hash_str

--------------------------------------------------------------------------------
