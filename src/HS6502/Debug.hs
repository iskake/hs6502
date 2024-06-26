module HS6502.Debug where

import Data.Word
import Memory
import HS6502

data Instruction = Instruction 
    { instName :: Inst
    , addrMode :: AddrMode
    , instArg :: Maybe (Either Word8 Word16)}

instance Show Instruction where
    show (Instruction inst aMode value) = show inst <> " " <> addrModeShow aMode value

addrModeShow :: AddrMode -> Maybe (Either Word8 Word16) -> String
addrModeShow Imm  (Just val) = "#$" <> hex8Or16 val
addrModeShow ZP   (Just val) = "$" <> hex8Or16 val
addrModeShow ZPX  (Just val) = "$" <> hex8Or16 val <> ",x"
addrModeShow ZPY  (Just val) = "$" <> hex8Or16 val <> ",y"
addrModeShow Rel  (Just val) = "$" <> hex8Or16 val
addrModeShow Abs  (Just val) = "$" <> hex8Or16 val
addrModeShow AbsX (Just val) = "$" <> hex8Or16 val <> ",x"
addrModeShow AbsY (Just val) = "$" <> hex8Or16 val <> ",y"
addrModeShow Ind  (Just val) = "($" <> hex8Or16 val <> ")"
addrModeShow IndX (Just val) = "($" <> hex8Or16 val <> ",x)"
addrModeShow IndY (Just val) = "($" <> hex8Or16 val <> "),y"
addrModeShow _ _ = ""

hex8Or16 :: Either Word8 Word16 -> String
hex8Or16 (Left  a) = hex8 a
hex8Or16 (Right b) = hex16 b

printCPUState :: Memory a => CPUState a -> String
printCPUState (CPUState a x y p sp pc _) = "A:" <> hex8 a <> " X:" <> hex8 x <> " Y:" <> hex8 y <> " P:" <> show p <> " SP:" <> hex8 sp <> " PC:" <> hex16 pc

printNextInstr :: Memory a => CPUState a -> String
printNextInstr (CPUState _ _ _ _ _ pc mem) = "  ->" <> hex16 pc <> ": " <> show (head (disasSect pc (pc+1) mem))

disasOp :: Word8 -> (Inst, AddrMode, Word16 -> Maybe (Either Word8 Word16))
disasOp op = (opToInst op, aMode, val)
    where
        aMode = opToAddrMode op
        val = case addrModeArgCount aMode of
                1 -> Just . Left . w8
                2 -> Just . Right
                _ -> const Nothing

disasOpToInst :: Word8 -> Word16 -> Instruction
disasOpToInst op addr = Instruction i a v
    where
        (i,a,f) = disasOp op
        v = f addr

printInstFromOp :: Word8 -> IO ()
printInstFromOp x = if i == ILL then return () else putStrLn ("$" <> hex8 x <> ": " <> show ins)
    where
        ins@(Instruction i _ _) = disasOpToInst x 0xffff

disasSect :: Memory a => Word16 -> Word16 -> a -> [Instruction]
disasSect from to mem | from >= to = []
                      | otherwise = (Instruction i a (f next)) : disasSect (from + (1 + addrModeArgCount a)) to mem
                      where
                        op = readAddr mem from
                        next = asAddress (readAddr mem (from+1)) (readAddr mem (from+2))
                        (i, a, f) = disasOp op

asList :: Memory a => Word16 -> CPUState a -> [Word8]
asList startAddr c = addrRead c startAddr : asList (startAddr+1) c

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