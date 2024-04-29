module Memory where

import Data.Word
import Data.Int
import Data.Bits
import Text.Printf (printf)

class Memory a where
    readAddr  :: a -> Word16 -> Word8
    writeAddr :: a -> Word16 -> Word8 -> a

-- Bit manipulation

-- | Combines two `Word8`s into a `Word16`s (a short 'memory address')
-- 
-- The first argument being the least siginificant byte, and the second the most significant byte.
asAddress :: Word8 -> Word8 -> Word16
asAddress least most = fromIntegral least .|. (fromIntegral most .<<. 8)

fromAddress :: Word16 -> [Word8]
fromAddress x = [w8 (x .&. 0xff), w8 $ (x .&. 0xff00) .>>. 8]

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

-- | Convert a number to a `Int8`
--
-- Same as `fromIntegral`
s8 :: Integral a => a -> Int8
s8 = fromIntegral

-- | Get the number as a 8-bit hexadecimal string with leading zeroes
hex8 :: Word8 -> String
hex8 = printf "%02x"

-- | Get the number as a 16-bit hexadecimal string with leading zeroes
hex16 :: Word16 -> String
hex16 = printf "%04x"

-- | Get the number as a 8-bit binary string with leading zeroes
bin8 :: Word8 -> String
bin8 = printf "%08b"