
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

runSimpleTestBytes :: Verbosity -> FilePath -> SimpleHashTest -> IO ()
runSimpleTestBytes = runSimpleTest' mkInput where
  mkInput :: String -> Either String ([Int], HsInput)
  mkInput input_str = Right ([length input_bytes], HsInput input_bytes) where
    input_bytes = map ordAscii input_str :: [Word8]

runSimpleTestBits :: Verbosity -> FilePath -> SimpleHashTest -> IO ()
runSimpleTestBits = runSimpleTest' mkInput where
  mkInput :: String -> Either String ([Int], HsInput)
  mkInput input_str = Right ([length input_bits], HsInput input_bits) where
    input_bytes = map ordAscii input_str :: [Word8]
    input_bits  = concatMap byteToBitsBE input_bytes :: [Bit]

--------------------------------------------------------------------------------

-- 625 bytes = 5000 bits output hardwired for now
runShakeTest :: Verbosity -> FilePath -> SimpleHashTest -> IO ()
runShakeTest = runSimpleTest' mkInput where
  mkInput :: String -> Either String ([Int], HsInput)
  mkInput input_str = Right ([length input_bytes, 625], HsInput input_bytes) where
    input_bytes = map ordAscii input_str :: [Word8]

--------------------------------------------------------------------------------

to_8_bytes_BE :: Integer -> [Word8]
to_8_bytes_BE n = [ byte (7-i) | i <- [0..7] ] where
  byte :: Int -> Word8
  byte k = fromIntegral (shiftR n (8*k) .&. 0xff)

to_16_bytes_BE :: Integer -> [Word8]
to_16_bytes_BE n = [ byte (15-i) | i <- [0..15] ] where
  byte :: Int -> Word8
  byte k = fromIntegral (shiftR n (8*k) .&. 0xff)

padBytes_SHA256 :: [Word8] -> [Word8]
padBytes_SHA256 bs = bs ++ padding where
  nbytes = length bs
  bitlen = 8 * nbytes
  chunks = div (bitlen + 8 + 64 + 511) 512
  final  = 64*chunks
  padding = 0x80 : replicate (final - nbytes - 9) 0 ++ to_8_bytes_BE (fromIntegral bitlen)

padBytes_SHA512 :: [Word8] -> [Word8]
padBytes_SHA512 bs = bs ++ padding where
  nbytes = length bs
  bitlen = 8 * nbytes
  chunks = div (bitlen + 8 + 128 + 1023) 1023
  final  = 128*chunks
  padding = 0x80 : replicate (final - nbytes - 17) 0 ++ to_16_bytes_BE (fromIntegral bitlen)

----------------------------------------

runChunkTest_SHA256 :: Verbosity -> FilePath -> SimpleHashTest -> IO ()
runChunkTest_SHA256 verbosity rootdir test = runSimpleTest'' checkResult mkInput verbosity rootdir test where

  mkInput :: String -> Either String ([Int], HsInput)
  mkInput input_str = 
    if orig_bitlen <= 512-64-1
      then Right ([], HsInput input_bits) 
      else Left "test skipped, because the input is too big for hash_chunk test"
    where
      input_bytes = map ordAscii input_str :: [Word8]
      input_bits  = concatMap byteToBitsBE $ padBytes_SHA256 input_bytes :: [Bit]
      orig_bitlen = 8 * length input_bytes

  checkResult wtns expected_output_str = do
    let result     = extractBits wtns ("main." ++ __outputSignal test)
        hash_bytes = map byteFromBitsBE $ partition 8 result
        hash_str   = concatMap toHexStringBE hash_bytes

    if hash_str == expected_output_str
      then putStrLn "OK."
      else do
        putStrLn "FAILED!"
        putStrLn $ " - expected = " ++ show expected_output_str
        putStrLn $ " - actual   = " ++ show hash_str

runChunkTest_SHA512 :: Verbosity -> FilePath -> SimpleHashTest -> IO ()
runChunkTest_SHA512 verbosity rootdir test = runSimpleTest'' checkResult mkInput verbosity rootdir test where

  mkInput :: String -> Either String ([Int], HsInput)
  mkInput input_str = 
    if orig_bitlen <= 1024-128-1
      then Right ([], HsInput input_bits) 
      else Left "test skipped, because the input is too big for hash_chunk test"
    where
      input_bytes = map ordAscii input_str :: [Word8]
      input_bits  = concatMap byteToBitsBE $ padBytes_SHA512 input_bytes :: [Bit]
      orig_bitlen = 8 * length input_bytes

  checkResult wtns expected_output_str = do
    let result     = extractBits wtns ("main." ++ __outputSignal test)
        hash_bytes = map byteFromBitsBE $ partition 8 result
        hash_str   = concatMap toHexStringBE hash_bytes

    if hash_str == expected_output_str
      then putStrLn "OK."
      else do
        putStrLn "FAILED!"
        putStrLn $ " - expected = " ++ show expected_output_str
        putStrLn $ " - actual   = " ++ show hash_str

--------------------------------------------------------------------------------

runSimpleTest' 
  :: (String -> Either String ([Int], HsInput)) 
  -> Verbosity -> FilePath -> SimpleHashTest -> IO ()
runSimpleTest' mkInput verbosity rootdir test = runSimpleTest'' checkResult mkInput verbosity rootdir test where

  checkResult wtns expected_output_str = do

    let result   = extractBytes wtns ("main." ++ __outputSignal test)
    let hash_str = concatMap toHexStringBE result

    if hash_str == expected_output_str
      then putStrLn "OK."
      else do
        putStrLn "FAILED!"
        putStrLn $ " - expected = " ++ show expected_output_str
        putStrLn $ " - actual   = " ++ show hash_str

----------------------------------------

runSimpleTest'' 
  :: (Witness Name Integer -> String -> IO ()) 
  -> (String -> Either String ([Int], HsInput)) 
  -> Verbosity -> FilePath -> SimpleHashTest -> IO ()
runSimpleTest'' checkResult mkInput verbosity rootdir test = do

  putStrLn $ "\nrunning tests for template `" ++ __templateName test ++ "`"

  let circom   = rootdir </> __circomFile test

  forM_ (zip [1..] (__testCases test)) $ \(idx, (input_str, expected_output_str)) -> do

    putStr $ "test case #" ++ show idx ++ ": "
    hFlush stdout

    case mkInput input_str of
      Left  msg -> putStrLn msg
      Right (template_params, hsinput) -> do

        if null input_str

          then do
            putStrLn "test for empty input string is skipped because of a bug in circom"

          else do
            let maincomp = MainComponent 
                  { _templateName   = __templateName test
                  , _templateParams = template_params
                  , _publicInputs   = [__inputSignal test]
                  }
          
            circuitfiles <- compileCircomCircuit verbosity circom (Just maincomp)
            
            let hsinputs =  HsInputs $ Map.fromList
                  [ (__inputSignal test , hsinput)
                  ] 
        
            witnessfiles <- computeWitness verbosity circuitfiles hsinputs
            wtns <- loadWitness_ verbosity circuitfiles witnessfiles
        
            checkResult wtns expected_output_str

--------------------------------------------------------------------------------
