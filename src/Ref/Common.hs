
{-# LANGUAGE TypeApplications #-}
module Ref.Common where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Char
import Data.List
import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

--------------------------------------------------------------------------------

class ToHexString a where
  toHexString :: a -> String

instance ToHexString Word8 where
  toHexString w = showNibble (shiftR w 4) : showNibble (w .&. 15) : []

instance ToHexString Word16 where
  toHexString w = 
    (toHexString @Word8 (fromIntegral (shiftR w 8))) ++ 
    (toHexString @Word8 (fromIntegral (w .&. 0xff)))

instance ToHexString Word32 where
  toHexString w = 
    (toHexString @Word16 (fromIntegral (shiftR w 16 ))) ++ 
    (toHexString @Word16 (fromIntegral (w .&. 0xffff)))

instance ToHexString Word64 where
  toHexString w = 
    (toHexString @Word32 (fromIntegral (shiftR w 32     ))) ++ 
    (toHexString @Word32 (fromIntegral (w .&. 0xffffffff)))

showNibble :: Word8 -> Char
showNibble k
  | k < 10    = chr (48 + fromIntegral k)
  | k < 16    = chr (87 + fromIntegral k)
  | otherwise = error "showNibble: outside of range"

--------------------------------------------------------------------------------

class ToByteString a where
  toByteStringBE :: a -> ByteString

instance ToByteString Word8 where
  toByteStringBE w = B.singleton w

instance ToByteString Word16 where
  toByteStringBE w = B.pack [ fromIntegral (shiftR w 8) , fromIntegral (w .&. 0xff) ]

instance ToByteString Word32 where
  toByteStringBE w = B.append 
    (toByteStringBE @Word16 (fromIntegral (shiftR w 16 ))) 
    (toByteStringBE @Word16 (fromIntegral (w .&. 0xffff)))

instance ToByteString Word64 where
  toByteStringBE w = B.append 
    (toByteStringBE @Word32 (fromIntegral (shiftR w 32     ))) 
    (toByteStringBE @Word32 (fromIntegral (w .&. 0xffffffff)))

--------------------------------------------------------------------------------

class FromByteString a where
  fromByteStringBE :: ByteString -> (a,ByteString)

fromByteStringBE_ :: FromByteString a => ByteString -> a
fromByteStringBE_ bs = case fromByteStringBE bs of
  (y,rest) -> if B.null rest 
    then y 
    else error "fromByteStringBE_: cannot parse"
    
instance FromByteString Word8 where
  fromByteStringBE bs = case B.unpack (B.take 1 bs) of
    [a] -> ( a , B.drop 1 bs )
    _   -> error "fromByteStringBE/Word8: unexpected end of input"

instance FromByteString Word16 where
  fromByteStringBE bs = case B.unpack (B.take 2 bs) of
    [a,b] -> let y = shiftL (fromIntegral a) 8 .|. (fromIntegral b) 
             in  ( y , B.drop 2 bs )
    _   -> error "fromByteStringBE/Word16: unexpected end of input"

instance FromByteString Word32 where
  fromByteStringBE bs = case B.unpack (B.take 4 bs) of
    [a,b,c,d] -> let y = shiftL (fromIntegral a) 24 .|. 
                         shiftL (fromIntegral b) 16 .|. 
                         shiftL (fromIntegral c)  8 .|. 
                                (fromIntegral d) 
                 in  ( y , B.drop 4 bs )
    _   -> error "fromByteStringBE/Word32: unexpected end of input"

--------------------------------------------------------------------------------
