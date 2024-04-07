module Memory where

import Data.Word

class Memory a where
    readAddr  :: a -> Word16 -> Word8
    writeAddr :: a -> Word16 -> Word8 -> a