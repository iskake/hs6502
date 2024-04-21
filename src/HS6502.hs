{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module HS6502 where
import Data.Word
import Control.Monad.State
import Control.Monad.Except
import Data.Vector hiding ((++),modify,elem)

import qualified Data.ByteString as B
import Data.Bits

import Memory


-- CPU needs:
-- registers
-- - A, X, Y, P, SP :: Word8
-- - PC  :: Word16
-- memory - Word16 indexing, Word8 elements
-- mmio?
type Register8 = Word8
type Register16 = Word16

-- | The state of the CPU
data (Memory a) => CPUState a = CPUState { rA  :: Register8  -- Accumulator
                                         , rX  :: Register8  -- X index register
                                         , rY  :: Register8  -- Y index register
                                         , rP  :: ProcStatus -- Processor status
                                         , rSP :: Register8  -- Stack pointer
                                         , rPC :: Register16 -- Program counter
                                         , cMem :: a        -- Program memory
                                         }
                                         deriving (Show)

-- | Processor status
--
-- Holds several flags 
data ProcStatus = ProcStatus { fC :: Bool    -- carry flag
                             , fZ :: Bool    -- zero flag
                             , fI :: Bool    -- interrupt disable
                             , fD :: Bool    -- decimal mode flag
                             , fB :: Bool    -- break command
                             , fV :: Bool    -- overflow flag
                                             -- nothing, always returns 1
                             , fN :: Bool    -- negative flag
                             }

instance Show ProcStatus where
    show p = bin $ destructP p

constructP :: Word8 -> ProcStatus
constructP val = ProcStatus (w8ToB (val .&. 0x01))
                            (w8ToB ((val .&. 0x02) .>>. 1))
                            (w8ToB ((val .&. 0x04) .>>. 2))
                            (w8ToB ((val .&. 0x08) .>>. 3))
                            (w8ToB ((val .&. 0x10) .>>. 4))
                            -- ...
                            (w8ToB ((val .&. 0x40) .>>. 6))
                            (w8ToB (val .>>. 7))

destructP :: ProcStatus -> Word8
destructP (ProcStatus c z i d b v n) = bToW8 c .|.
                                       (bToW8 z .<<. 1) .|.
                                       (bToW8 i .<<. 2) .|.
                                       (bToW8 d .<<. 3) .|.
                                       (bToW8 b .<<. 4) .|.
                                             (1 .<<. 5) .|.
                                       (bToW8 v .<<. 6) .|.
                                       (bToW8 n .<<. 7)

-- TODO: Replace with Array or MVector?
newtype U8Memory = U8Memory (Vector Word8) --MVector Word16 Word8
    deriving (Show)

instance Memory U8Memory where
    readAddr :: U8Memory -> Word16 -> Word8
    readAddr (U8Memory mem) addr = mem ! fromIntegral addr
    writeAddr :: U8Memory -> Word16 -> Word8 -> U8Memory -- TODO?
    writeAddr (U8Memory mem) addr val = U8Memory (mem // [(fromIntegral addr, val)])

-- TODO: replace temporary intial memory with real implementation
initMem :: U8Memory
initMem = U8Memory (fromList [0xa9, 0x77, 0x34])

-- TODO: make this actually work
type CPU' a = ExceptT (B.ByteString) (StateT (CPUState U8Memory) IO) a
type CPU = CPU' ()

type CPUState' = CPUState U8Memory

emptyState :: CPUState'
emptyState = CPUState 0 0 0 (constructP 0) 0 0 initMem

-- stepCPU :: CPU -> Memory -> CPU
stepCPU = runStateT . runExceptT

-- getCPUState :: CPU -> CPUState
-- getCPUState cpu = do
--     s' <- gets



---------------------------------------
-- 
-- CPU register and memory manipulation
-- 
---------------------------------------

pcInc :: Memory a => (CPUState a -> b) -> Word16 -> CPUState a -> (b, CPUState a)
pcInc f i c@(CPUState _ _ _ _ _ pc _) = (f c, c { rPC = pc+i })

-- Reading from memory
-- -------------------
pcRead :: Memory a => CPUState a -> Word8
pcRead (CPUState _ _ _ _ _ pc mem) = readAddr mem pc

pcReadInc :: Memory a => CPUState a -> (Word8, CPUState a)
pcReadInc = pcInc pcRead 1


pcRead16 :: Memory a => CPUState a -> Word16
pcRead16 (CPUState _ _ _ _ _ pc mem) = asAddress (readAddr mem pc) (readAddr mem (pc+1))

pcRead16Inc :: Memory a => CPUState a -> (Word16, CPUState a)
pcRead16Inc = pcInc pcRead16 2
-- pcRead16Inc c@(CPUState _ _ _ _ _ pc _) = (pcRead16 c, c { rPC = pc+2 })


absRead :: Memory a => IndexRegister -> CPUState a -> Word8
absRead ir c@(CPUState _ _ _ _ _ _ mem) = readAddr mem (pcRead16 c + w16 (idx ir c))

absReadInc :: Memory a => IndexRegister -> CPUState a -> (Word8, CPUState a)
absReadInc ir = pcInc (absRead ir) 2


-- zeroPageRead :: Memory a => CPUState a -> Word8
-- zeroPageRead c@(CPUState _ _ _ _ _ _ mem) = readAddr mem (asAddress (pcRead c) 0x00)

-- zeroPageReadInc :: Memory a => CPUState a -> (Word8, CPUState a)
-- zeroPageReadInc = pcInc zeroPageRead 1

zeroPageRead :: Memory a => IndexRegister -> CPUState a -> Word8
zeroPageRead ir c@(CPUState _ _ _ _ _ _ mem) = readAddr mem (asAddress (pcRead c + (if ir == X then rX c else rY c)) 0x00)

zeroPageReadInc :: Memory a => IndexRegister -> CPUState a -> (Word8, CPUState a)
zeroPageReadInc ir = pcInc (zeroPageRead ir) 1


getIndirect :: Memory a => Word16 -> CPUState a -> Word16
getIndirect addr c@(CPUState _ _ _ _ _ _ mem) = asAddress (readAddr mem addr) (readAddr mem (addr + 1))

indRead :: Memory a => IndexRegister -> CPUState a -> Word8
indRead None c@(CPUState _ _ _ _ _ _ mem) = undefined -- ?TODO? only used for JMP => no need for reading?
indRead X    c@(CPUState _ _ _ _ _ _ mem) = readAddr mem (getIndirect ptr c)
    where
        ptr = asAddress (pcRead c + w8 (idx X c)) 0x00
indRead Y    c@(CPUState _ _ _ _ _ pc mem) = readAddr mem (getIndirect ptr c + w16 (idx Y c))
    where
        ptr = asAddress (readAddr mem pc) 0x00

indReadInc :: Memory a => IndexRegister -> CPUState a -> (Word8, CPUState a)
indReadInc ir = pcInc (indRead ir) 1


-- Writing to memory
-- -----------------
addrWrite :: Memory a => CPUState a -> Word16 -> Word8 -> CPUState a
addrWrite c@(CPUState _ _ _ _ _ _ mem) addr val = c { cMem = writeAddr mem addr val}

idx :: Memory a => IndexRegister -> CPUState a -> Word8
idx None _ = 0
idx X c = fromIntegral (rX c)
idx Y c = fromIntegral (rY c)

absWrite :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
absWrite ir c = addrWrite c (pcRead16 c + w16 (idx ir c))

absWriteInc :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
absWriteInc ir c = addrWrite newc (addr + w16 (idx ir newc))
    where
        (addr, newc) = pcRead16Inc c


zpWrite :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
zpWrite ir c = addrWrite c (asAddress (pcRead c + idx ir c) 0x00)

zpWriteInc :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
zpWriteInc ir c = addrWrite newc (asAddress (addr + idx ir c) 0x00)
    where
        (addr, newc) = pcReadInc c

-- TODO
-- indWrite :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
-- indWrite ir c = addrWrite c (asAddress (pcRead c + idx ir c) 0x00)

-- indWriteInc :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
-- indWriteInc ir c = addrWrite newc (asAddress (addr + idx ir c) 0x00)
--     where
--         (addr, newc) = pcReadInc c

-- Stack
-- | Push one byte onto the stack, decrementing the stack pointer.
pushByte :: Memory a => CPUState a -> Word8 -> CPUState a
pushByte c@(CPUState _ _ _ _ sp _ mem) val = c {rSP = sp-1, cMem = writeAddr mem (asAddress sp 0x01) val }

-- | Pull one byte from the stack, incrementing the stack pointer.
pullByte :: Memory a => CPUState a -> (Word8, CPUState a)
pullByte c@(CPUState _ _ _ _ sp _ mem) = (readAddr mem (asAddress sp 0x01), c {rSP = sp+1})

---------------------------------------
--
-- CPU instructions
-- 
---------------------------------------

type Opcode = Word8

data Inst = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK | BVC | BVS | CLC
          | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR | INC | INX | INY | JMP
          | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL | ROR | RTI
          | RTS | SBC | SEC | SED | SEI | STA | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
          | ILL
          deriving (Show)

data AddrMode = Imp | Acc | Imm | ZP | ZPX | ZPY | Abs | AbsX | AbsY | Ind | IndX | IndY
              deriving (Show)

data IndexRegister = None   -- No indexing
                   | X      -- X register
                   | Y      -- Y register
                    deriving Eq
{- 
Opcode matrix:
[BRK,ORA,ILL,ILL,ILL,ORA,ASL,ILL,PHP,ORA,ASL,ILL,ILL,ORA,ASL,ILL,BPL,ORA,ILL,ILL,ILL,ORA,ASL,ILL,CLC,ORA,ILL,ILL,ILL,ORA,ASL,ILL
,JSR,AND,ILL,ILL,BIT,AND,ROL,ILL,PLP,AND,ROL,ILL,BIT,AND,ROL,ILL,BMI,AND,ILL,ILL,ILL,AND,ROL,ILL,SEC,AND,ILL,ILL,ILL,AND,ROL,ILL
,RTI,EOR,ILL,ILL,ILL,EOR,LSR,ILL,PHA,EOR,LSR,ILL,JMP,EOR,LSR,ILL,BVC,EOR,ILL,ILL,ILL,EOR,LSR,ILL,CLI,EOR,ILL,ILL,ILL,EOR,LSR,ILL
,RTS,ADC,ILL,ILL,ILL,ADC,ROR,ILL,PLA,ADC,ROR,ILL,JMP,ADC,ROR,ILL,BVS,ADC,ILL,ILL,ILL,ADC,ROR,ILL,SEI,ADC,ILL,ILL,ILL,ADC,ROR,ILL
,ILL,STA,ILL,ILL,STY,STA,STX,ILL,DEY,ILL,TXA,ILL,STY,STA,STX,ILL,BCC,STA,ILL,ILL,STY,STA,STX,ILL,TYA,STA,TXS,ILL,ILL,STA,ILL,ILL
,LDX,LDA,LDX,ILL,LDX,LDA,LDX,ILL,TAY,LDA,TAX,ILL,LDX,LDA,LDX,ILL,BCS,LDA,ILL,ILL,LDX,LDA,LDX,ILL,CLV,LDA,TSX,ILL,LDX,LDA,LDX,ILL
,CPY,CMP,ILL,ILL,CPY,CMP,DEC,ILL,INY,CMP,DEX,ILL,CPY,CMP,DEC,ILL,BNE,CMP,ILL,ILL,ILL,CMP,DEC,ILL,CLD,CMP,ILL,ILL,ILL,CMP,DEC,ILL
,CPX,SBC,ILL,ILL,CPX,SBC,INC,ILL,INX,ILL,NOP,ILL,CPX,SBC,INC,ILL,BEQ,SBC,ILL,ILL,ILL,SBC,INC,ILL,SED,SBC,ILL,ILL,ILL,SBC,INC,ILL]
 -}

-- | Get the instruction corresponding to the given opcode.
opToInst :: Opcode -> Inst
opToInst 0x90 = BCC
opToInst 0xb0 = BCS
opToInst 0xf0 = BEQ
opToInst 0x30 = BMI
opToInst 0xd0 = BNE
opToInst 0x10 = BPL
opToInst 0x00 = BRK
opToInst 0x50 = BVC
opToInst 0x70 = BVS
opToInst 0x18 = CLC
opToInst 0xd8 = CLD
opToInst 0x58 = CLI
opToInst 0xb8 = CLV
opToInst 0xca = DEX
opToInst 0x88 = DEY
opToInst 0x20 = JSR
opToInst 0xe8 = INX
opToInst 0xc8 = INY
opToInst 0xea = NOP
opToInst 0x48 = PHA
opToInst 0x08 = PHP
opToInst 0x68 = PLA
opToInst 0x28 = PLP
opToInst 0x40 = RTI
opToInst 0x60 = RTS
opToInst 0x38 = SEC
opToInst 0xf8 = SED
opToInst 0x78 = SEI
opToInst 0xaa = TAX
opToInst 0xa8 = TAY
opToInst 0xba = TSX
opToInst 0x8a = TXA
opToInst 0x9a = TXS
opToInst 0x98 = TYA
-- TODO: use pattern matching instead only...?
opToInst op | op `elem` [0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71] = ADC
            | op `elem` [0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31] = AND
            | op `elem` [0x0a, 0x06, 0x16, 0x0e, 0x1e]                   = ASL
            | op `elem` [0x24, 0x2c]                                     = BIT
            | op `elem` [0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1] = CMP
            | op `elem` [0xe0, 0xe4, 0xec]                               = CPX
            | op `elem` [0xc0, 0xc4, 0xcc]                               = CPY
            | op `elem` [0xc6, 0xd6, 0xce, 0xde]                         = DEC
            | op `elem` [0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51] = EOR
            | op `elem` [0xe6, 0xf6, 0xee, 0xfe]                         = INC
            | op `elem` [0x4c, 0x6c]                                     = JMP
            | op `elem` [0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1] = LDA
            | op `elem` [0xa2, 0xa6, 0xb6, 0xae, 0xbe]                   = LDX
            | op `elem` [0xa0, 0xa4, 0xb4, 0xac, 0xbc]                   = LDX
            | op `elem` [0x4a, 0x46, 0x56, 0x4e, 0x5e]                   = LSR
            | op `elem` [0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11] = ORA
            | op `elem` [0x2a, 0x26, 0x36, 0x2e, 0x3e]                   = ROL
            | op `elem` [0x6a, 0x66, 0x76, 0x6e, 0x7e]                   = ROR
            | op `elem` [0x39, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1] = SBC
            | op `elem` [0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91]       = STA
            | op `elem` [0x86, 0x96, 0x8e]                               = STX
            | op `elem` [0x84, 0x94, 0x8c]                               = STY
opToInst _ = ILL

-- | Run a specific instruction on the cpu state
runInst :: Memory a => Inst -> AddrMode -> CPUState a -> CPUState a
runInst ADC mode c = undefined
runInst AND mode c = undefined
runInst ASL mode c = undefined
runInst BCC mode c = undefined
runInst BCS mode c = undefined
runInst BEQ mode c = undefined
runInst BIT mode c = undefined
runInst BMI mode c = undefined
runInst BNE mode c = undefined
runInst BPL mode c = undefined
runInst BRK _ c = undefined
runInst BVC mode c = undefined
runInst BVS mode c = undefined
runInst CLC mode c = undefined
runInst CLD mode c = undefined
runInst CLI mode c = undefined
runInst CLV mode c = undefined
runInst CMP mode c = undefined
runInst CPX mode c = undefined
runInst CPY mode c = undefined
runInst DEC mode c = undefined
runInst DEX mode c = undefined
runInst DEY mode c = undefined
runInst EOR mode c = undefined
runInst INC mode c = undefined
runInst INX mode c = undefined
runInst INY mode c = undefined
runInst JMP mode c = do
    let (val,newc) = case mode of
                    Ind -> undefined    -- TODO
                    Abs -> pcRead16Inc c
                    _   -> error "Unreachable"
    newc {rPC = val}
runInst JSR mode c = do
    let pc = rPC c
    let (addr, newc) = pcRead16Inc c
    let newc' = pushByte newc (lsb pc)
    let newc'' = pushByte newc' (msb pc)
    newc'' {rPC = addr}
runInst LDA mode c = do
    let (val,newc) = case mode of
                    Imm  -> pcReadInc c
                    ZP   -> zeroPageReadInc None c
                    ZPX  -> zeroPageReadInc X c
                    Abs  -> absReadInc None c
                    AbsX -> absReadInc X c
                    AbsY -> absReadInc Y c
                    IndX -> indReadInc X c
                    IndY -> indReadInc Y c
                    _    -> error "Unreachable"
    let z = val == 0
    let n = ((val .&. 0x80) .>>. 7) == 1
    let newP = (rP newc) {fZ = z, fN = n}
    newc {rA = val, rP = newP}
    -- (setP newP (setA val newc))
    -- return (setP newP (setA val newc))
runInst LDX mode c = do
    let (val,newc) = case mode of
                    Imm  -> pcReadInc c
                    ZP   -> zeroPageReadInc None c
                    ZPY  -> zeroPageReadInc Y c
                    Abs  -> absReadInc None c
                    AbsY -> absReadInc Y c
                    _    -> error "Unreachable"
    let z = val == 0
    let n = ((val .&. 0x80) .>>. 7) == 1
    let newP = (rP newc) {fZ = z, fN = n}
    newc {rX = val, rP = newP}
runInst LDY mode c = do
    let (val,newc) = case mode of
                    Imm  -> pcReadInc c
                    ZP   -> zeroPageReadInc None c
                    ZPX  -> zeroPageReadInc X c
                    Abs  -> absReadInc None c
                    AbsX -> absReadInc X c
                    _    -> error "Unreachable"
    let z = val == 0
    let n = ((val .&. 0x80) .>>. 7) == 1
    let newP = (rP newc) {fZ = z, fN = n}
    newc {rY = val, rP = newP}
runInst LSR mode c = undefined
runInst NOP _ c = c  -- TODO: newc because of cycles? already handled when decoding opcode?
runInst ORA mode c = undefined
runInst PHA mode c = undefined
runInst PHP mode c = undefined
runInst PLA mode c = undefined
runInst PLP mode c = undefined
runInst ROL mode c = undefined
runInst ROR mode c = undefined
runInst RTI mode c = undefined
runInst RTS mode c = undefined
runInst SBC mode c = undefined
runInst SEC mode c = undefined
runInst SED mode c = undefined
runInst SEI mode c = undefined
runInst STA mode c = (case mode of
                        ZP   -> zpWrite None
                        ZPX  -> zpWrite X
                        Abs  -> absWriteInc None
                        AbsX -> absWriteInc X
                        AbsY -> absWriteInc Y
                        IndX -> undefined
                        IndY -> undefined
                        _    -> error "Unreachable") c (rA c)
runInst STX mode c = (case mode of
                        ZP   -> zpWrite None
                        ZPY  -> zpWrite Y
                        Abs  -> absWriteInc None
                        _    -> error "Unreachable") c (rX c)
runInst STY mode c = (case mode of
                        ZP   -> zpWrite None
                        ZPX  -> zpWrite X
                        Abs  -> absWriteInc None
                        _    -> error "Unreachable") c (rY c)
runInst TAX mode c = undefined
runInst TAY mode c = undefined
runInst TSX mode c = undefined
runInst TXA mode c = undefined
runInst TXS mode c = undefined
runInst TYA mode c = undefined
runInst ILL mode c = error $ "Undefined instruction"