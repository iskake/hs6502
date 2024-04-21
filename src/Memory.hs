module Memory where

import Data.Word
import Data.Bits
import Numeric (showHex, showBin)

class Memory a where
    readAddr  :: a -> Word16 -> Word8
    writeAddr :: a -> Word16 -> Word8 -> a

-- Bit manipulation

-- | Represent two bytes (`Word8`) as a short 'memory address' (`Word16`)
-- 
-- The first argument being the least siginificant byte, and the second the most significant byte.
asAddress :: Word8 -> Word8 -> Word16
asAddress least most = fromIntegral least .|. (fromIntegral most .<<. 8)

-- | Get the Least Significant Byte of a `Word16`
lsb :: Word16 -> Word8
lsb = fromIntegral . (.&. 0xff)

-- | Get the Most Significant Byte of a `Word16`
msb :: Word16 -> Word8
msb = fromIntegral . (.>>. 8)

bToW8 :: Num a => Bool -> a
bToW8 False = 0
bToW8 True  = 1

w8ToB :: (Eq a, Num a) => a -> Bool
w8ToB 0 = False
w8ToB _ = True

w8 :: Integral a => a -> Word8
w8 = fromIntegral

w16 :: Integral a => a -> Word16
w16 = fromIntegral

hex :: Integral a => a -> String
hex = flip showHex ""

bin :: Integral a => a -> String
bin = flip showBin ""