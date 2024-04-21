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

hex :: Integral a => a -> String
hex = flip showHex ""

bin :: Integral a => a -> String
bin = flip showBin ""