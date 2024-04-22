module Memory where

import Data.Word
import Data.Bits
import Numeric (showHex, showBin)

class Memory a where
    readAddr  :: a -> Word16 -> Word8
    writeAddr :: a -> Word16 -> Word8 -> a

-- Bit manipulation

-- | Combines two `Word8`s into a `Word16`s (a short 'memory address')
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

-- | Go from a boolean value to a number
bToI :: Num a => Bool -> a
bToI False = 0
bToI True  = 1

-- | Go from a number to a boolean value
iToB :: (Eq a, Num a) => a -> Bool
iToB 0 = False
iToB _ = True

-- | Convert a number to a `Word8`
--
-- Same as `fromIntegral`
w8 :: Integral a => a -> Word8
w8 = fromIntegral

-- | Convert a number to a `Word16`
--
-- Same as `fromIntegral`
w16 :: Integral a => a -> Word16
w16 = fromIntegral


-- | Get the number as a hexidecimal string
hex :: Integral a => a -> String
hex = flip showHex ""

-- | Get the number as a binary string
bin :: Integral a => a -> String
bin = flip showBin ""