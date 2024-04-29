module HS6502.Debug where

import Data.Word
import Memory
import HS6502

data Instruction = Instruction 
    { instName :: Inst
    , addrMode :: AddrMode
    , instArg :: Maybe (Either Word8 Word16)}

instance Show Instruction where
    show (Instruction inst aMode value) = show inst ++ " " ++ show aMode ++ " " ++ show (hex8Or16 <$> value)

hex8Or16 :: Either Word8 Word16 -> String
hex8Or16 (Left  a) = hex8 a
hex8Or16 (Right b) = hex16 b

printCPUState :: Memory a => CPUState a -> String
printCPUState (CPUState a x y p sp pc _) = "A:" ++ hex8 a ++ " X:" ++ hex8 x ++ " Y:" ++ hex8 y ++ " P:" ++ show p ++ " SP:" ++ hex8 sp ++ " PC:" ++ hex16 pc

printNextInstr :: Memory a => CPUState a -> String
printNextInstr (CPUState _ _ _ _ _ pc mem) = "  ->" ++ hex16 pc ++ ": " ++ show (head (disasSect pc (pc+1) mem))

disasOp :: Word8 -> (Inst, AddrMode, Word16 -> Maybe (Either Word8 Word16))
disasOp op = (opToInst op, aMode, val)
    where
        aMode = opToAddrMode op
        val = case addrModeArgCount aMode of
                1 -> Just . Left . w8
                2 -> Just . Right
                _ -> const Nothing

disasSect :: Memory a => Word16 -> Word16 -> a -> [Instruction]
disasSect from to mem | from == to = []
                      | otherwise = (Instruction i a (f next)) : disasSect (from + (1 + addrModeArgCount a)) to mem
                      where
                        op = readAddr mem from
                        next = asAddress (readAddr mem (from+1)) (readAddr mem (from+2))
                        (i, a, f) = disasOp op

addrModeArgCount :: Num a => AddrMode -> a
addrModeArgCount Imp  = 0
addrModeArgCount Acc  = 0
addrModeArgCount Imm  = 1
addrModeArgCount ZP   = 1
addrModeArgCount ZPX  = 1
addrModeArgCount ZPY  = 1
addrModeArgCount Rel  = 1
addrModeArgCount Abs  = 2
addrModeArgCount AbsX = 2
addrModeArgCount AbsY = 2
addrModeArgCount Ind  = 2
addrModeArgCount IndX = 1
addrModeArgCount IndY = 1