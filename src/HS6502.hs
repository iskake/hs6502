{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module HS6502 where
import Data.Word
import Control.Monad.State
import Control.Monad.Except
import Data.Vector hiding ((++),modify,elem)

import Memory

-- import Data.Vector.Generic
import qualified Data.ByteString as B
import Data.Bits


-- CPU needs:
-- registers
-- - A, X, Y, P, SP :: Word8
-- - PC  :: Word16
-- memory - Word16 indexing, Word8 elements
-- mmio?
type Register8 = Word8
type Register16 = Word16

data CPUState = CPUState { rA  :: Register8 -- Accumulator
                         , rX  :: Register8 -- X index register
                         , rY  :: Register8 -- Y index register
                         , rP  :: Register8 -- Processor status
                         , rSP :: Register8 -- Stack pointer
                         , rPC :: Register16 -- Program counter
                         , cMem :: U8Memory -- ?'Replace' with `Memory a`? (I forgot how)
                         }

emptyState :: CPUState
emptyState = CPUState 0 0 0 0 0 0 initMem

type Opcode = Word8

-- TODO: Replace with Array or MVector?
type U8Memory = Vector Word8 --MVector Word16 Word8

instance Memory U8Memory where
    -- readAddr :: Memory -> Word16 -> Word8
    readAddr mem addr = mem ! fromIntegral addr
    -- writeAddr :: Memory -> Word16 -> Word8 -> Memory
    writeAddr  mem addr val = mem // [(fromIntegral addr, val)]

initMem :: U8Memory
initMem = fromList [0xa9, 0x11, 0x00]

type CPU' a = ExceptT (B.ByteString) (StateT CPUState IO) a
type CPU = CPU' ()

-- data CPUMem = CPUMem CPU Memory


-- stepCPU :: CPU -> Memory -> CPU
stepCPU = runStateT . runExceptT

-- General fuctions for handling CPU operations
-- getCPUState :: CPU -> CPUState
-- getCPUState cpu = do
--     s' <- gets

setA :: Word8 -> CPUState -> CPUState
setA val c = c {rA = val}

setX :: Word8 -> CPUState -> CPUState
setX val c = c {rX = val}

setY :: Word8 -> CPUState -> CPUState
setY val c = c {rY = val}
-- setY val c = CPUState (rA  c)
--                       (rX  c)
--                       val
--                       (rP  c)
--                       (rSP c)
--                       (rPC c)
--                       (cMem c)

setP :: Word8 -> CPUState -> CPUState
setP val c = c {rP = val}

setSP :: Word8 -> CPUState -> CPUState
setSP val c = c {rSP = val}

setPC :: Word16 -> CPUState -> CPUState
setPC val c = c {rPC = val}

-- ...

pcRead :: CPUState -> Word8
pcRead (CPUState _ _ _ _ _ pc mem) = readAddr mem pc

pcReadInc :: CPUState -> (Word8, CPUState)
pcReadInc c@(CPUState _ _ _ _ _ pc mem) = (pcRead c, setPC (pc+1) c)

absRead :: CPUState -> Word8
absRead c@(CPUState _ _ _ _ _ pc mem) = readAddr mem
                                        ((fromIntegral $ readAddr mem pc)
                                        .|.
                                        ((fromIntegral $ readAddr mem pc+1) .<<. 8))

absReadInc :: CPUState -> (Word8, CPUState)
absReadInc c@(CPUState _ _ _ _ _ pc mem) = (absRead c, setPC (pc+2) c)

-- CPU instructions:
-- -------------------

data Inst = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK | BVC | BVS | CLC
          | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR | INC | INX | INY | JMP
          | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL | ROR | RTI
          | RTS | SBC | SEC | SED | SEI | STA | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
          | ILL
          deriving (Show)
{- 
Instruction matrix:

[BRK,ORA,ILL,ILL,ILL,ORA,ASL,ILL,PHP,ORA,ASL,ILL,ILL,ORA,ASL,ILL,BPL,ORA,ILL,ILL,ILL,ORA,ASL,ILL,CLC,ORA,ILL,ILL,ILL,ORA,ASL,ILL
,JSR,AND,ILL,ILL,BIT,AND,ROL,ILL,PLP,AND,ROL,ILL,BIT,AND,ROL,ILL,BMI,AND,ILL,ILL,ILL,AND,ROL,ILL,SEC,AND,ILL,ILL,ILL,AND,ROL,ILL
,RTI,EOR,ILL,ILL,ILL,EOR,LSR,ILL,PHA,EOR,LSR,ILL,JMP,EOR,LSR,ILL,BVC,EOR,ILL,ILL,ILL,EOR,LSR,ILL,CLI,EOR,ILL,ILL,ILL,EOR,LSR,ILL
,RTS,ADC,ILL,ILL,ILL,ADC,ROR,ILL,PLA,ADC,ROR,ILL,JMP,ADC,ROR,ILL,BVS,ADC,ILL,ILL,ILL,ADC,ROR,ILL,SEI,ADC,ILL,ILL,ILL,ADC,ROR,ILL
,ILL,STA,ILL,ILL,STY,STA,STX,ILL,DEY,ILL,TXA,ILL,STY,STA,STX,ILL,BCC,STA,ILL,ILL,STY,STA,STX,ILL,TYA,STA,TXS,ILL,ILL,STA,ILL,ILL
,LDX,LDA,LDX,ILL,LDX,LDA,LDX,ILL,TAY,LDA,TAX,ILL,LDX,LDA,LDX,ILL,BCS,LDA,ILL,ILL,LDX,LDA,LDX,ILL,CLV,LDA,TSX,ILL,LDX,LDA,LDX,ILL
,CPY,CMP,ILL,ILL,CPY,CMP,DEC,ILL,INY,CMP,DEX,ILL,CPY,CMP,DEC,ILL,BNE,CMP,ILL,ILL,ILL,CMP,DEC,ILL,CLD,CMP,ILL,ILL,ILL,CMP,DEC,ILL
,CPX,SBC,ILL,ILL,CPX,SBC,INC,ILL,INX,ILL,NOP,ILL,CPX,SBC,INC,ILL,BEQ,SBC,ILL,ILL,ILL,SBC,INC,ILL,SED,SBC,ILL,ILL,ILL,SBC,INC,ILL]
 -}

data AddrMode = Acc | Imm | ZP | ZPX | ZPY | Abs | AbsX | AbsY | IndX | IndY
              deriving (Show)

data Flag = C   -- carry flag
          | Z   -- zero flag
          | I   -- interrupt disable
          | D   -- decimal mode flag
          | B   -- break command
          | V   -- overflow flag
          | N   -- negative flag


opToInst :: Word8 -> Inst
-- TODO: use pattern matching instead only...?
opToInst op | op `elem` [0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71] = ADC
            | op `elem` [0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31] = AND
            | op `elem` [0x0a, 0x06, 0x16, 0x0e, 0x1e]                   = ASL
            | op `elem` [0x90]                                           = BCC
            | op `elem` [0xb0]                                           = BCS
            | op `elem` [0xf0]                                           = BEQ
            | op `elem` [0x24, 0x2c]                                     = BIT
            | op `elem` [0x30]                                           = BMI
            | op `elem` [0xd0]                                           = BNE
            | op `elem` [0x10]                                           = BPL
            | op `elem` [0x00]                                           = BRK
            | op `elem` [0x50]                                           = BVC
            | op `elem` [0x70]                                           = BVS
            | op `elem` [0x18]                                           = CLC
            | op `elem` [0xd8]                                           = CLD
            | op `elem` [0x58]                                           = CLI
            | op `elem` [0xb8]                                           = CLV
            | op `elem` [0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1] = CMP
            | op `elem` [0xe0, 0xe4, 0xec]                               = CPX
            | op `elem` [0xc0, 0xc4, 0xcc]                               = CPY
            | op `elem` [0xc6, 0xd6, 0xce, 0xde]                         = DEC
            | op `elem` [0xca]                                           = DEX
            | op `elem` [0x88]                                           = DEY
            | op `elem` [0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51] = EOR
            | op `elem` [0xe6, 0xf6, 0xee, 0xfe]                         = INC
            | op `elem` [0xe8]                                           = INX
            | op `elem` [0xc8]                                           = INY
            | op `elem` [0x4c, 0x6c]                                     = JMP
            | op `elem` [0x20]                                           = JSR
            | op `elem` [0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1] = LDA
            | op `elem` [0xa2, 0xa6, 0xb6, 0xae, 0xbe]                   = LDX
            | op `elem` [0xa0, 0xa4, 0xb4, 0xac, 0xbc]                   = LDX
            | op `elem` [0x4a, 0x46, 0x56, 0x4e, 0x5e]                   = LSR
            | op `elem` [0xea]                                           = NOP
            | op `elem` [0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11] = ORA
            | op `elem` [0x48]                                           = PHA
            | op `elem` [0x08]                                           = PHP
            | op `elem` [0x68]                                           = PLA
            | op `elem` [0x28]                                           = PLP
            | op `elem` [0x2a, 0x26, 0x36, 0x2e, 0x3e]                   = ROL
            | op `elem` [0x6a, 0x66, 0x76, 0x6e, 0x7e]                   = ROR
            | op `elem` [0x40]                                           = RTI
            | op `elem` [0x60]                                           = RTS
            | op `elem` [0x39, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1] = SBC
            | op `elem` [0x38]                                           = SEC
            | op `elem` [0xf8]                                           = SED
            | op `elem` [0x78]                                           = SEI
            | op `elem` [0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91]       = STA
            | op `elem` [0x86, 0x96, 0x8e]                               = STX
            | op `elem` [0x84, 0x94, 0x8c]                               = STY
            | op `elem` [0xaa]                                           = TAX
            | op `elem` [0xa8]                                           = TAY
            | op `elem` [0xba]                                           = TSX
            | op `elem` [0x8a]                                           = TXA
            | op `elem` [0x9a]                                           = TXS
            | op `elem` [0x98]                                           = TYA
            | otherwise                                                  = ILL

runInst :: Inst -> AddrMode -> CPUState -> CPUState
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
runInst BRK mode c = undefined
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
runInst JMP mode c = undefined
runInst JSR mode c = undefined
runInst LDA mode c = do
    -- c <- get
    let (val,newc) = case mode of
                    Imm  -> pcReadInc c
                    ZP   -> undefined
                    ZPX  -> undefined
                    Abs  -> absReadInc c
                    AbsX -> undefined
                    AbsY -> undefined
                    IndX -> undefined
                    IndY -> undefined
                    _    -> error "Unreachable"
    let z = (if val == 0 then 1 else 0) .<<. 5
    let n = (val .&. 0x80) .>>. 7
    let newP = (z .|. n .|. ((0x5e) .&. (rP newc)))
    (setP newP (setA val newc))
    -- return (setP newP (setA val newc))
runInst LDX mode c = do
    let (val,newc) = case mode of
                    Imm  -> pcReadInc c
                    ZP   -> undefined
                    ZPY  -> undefined
                    Abs  -> absReadInc c
                    AbsY -> undefined
                    _    -> error "Unreachable"
    let z = (if val == 0 then 1 else 0) .<<. 5
    let n = (val .&. 0x80) .>>. 7
    let newP = (z .|. n .|. ((0x5e) .&. (rP newc)))
    (setP newP (setX val newc))
    -- return (setP newP (setA val newc))
runInst LDY mode c = undefined
runInst LSR mode c = undefined
runInst NOP mode c = c
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
runInst STA mode c = undefined
runInst STX mode c = undefined
runInst STY mode c = undefined
runInst TAX mode c = undefined
runInst TAY mode c = undefined
runInst TSX mode c = undefined
runInst TXA mode c = undefined
runInst TXS mode c = undefined
runInst TYA mode c = undefined
runInst ILL mode c = error $ "Undefined instruction"
-- runInst _ = undefined