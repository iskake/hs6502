{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

module HS6502 where
import Data.Bits
import Data.Word
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Vector as V
import qualified Data.ByteString as B

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

emptyState :: Memory a => a -> CPUState a
emptyState = CPUState 0 0 0 (constructP 0b100) 0xff 0x8000

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
    show p = bin8 $ destructP p

constructP :: Word8 -> ProcStatus
constructP val = ProcStatus (val `testBit` 0)
                            (val `testBit` 1)
                            (val `testBit` 2)
                            (val `testBit` 3)
                            (val `testBit` 4)
                            (val `testBit` 6)
                            (val `testBit` 7)

destructP :: ProcStatus -> Word8
destructP (ProcStatus c z i d b v n) = bToI c .|.
                                       (bToI z .<<. 1) .|.
                                       (bToI i .<<. 2) .|.
                                       (bToI d .<<. 3) .|.
                                       (bToI b .<<. 4) .|.
                                            (1 .<<. 5) .|.
                                       (bToI v .<<. 6) .|.
                                       (bToI n .<<. 7)



-- TODO: Replace with Array or MVector?
newtype U8Memory = U8Memory (V.Vector Word8) --MVector Word16 Word8

instance Show U8Memory where
    show (U8Memory v) = "Memory of length " ++ show (V.length v)

instance Memory U8Memory where
    readAddr :: U8Memory -> Word16 -> Word8
    readAddr (U8Memory mem) addr = mem V.! fromIntegral addr
    writeAddr :: U8Memory -> Word16 -> Word8 -> U8Memory
    writeAddr m@(U8Memory mem) addr val = if addr < 0x8000
                                            then U8Memory (mem V.// [(fromIntegral addr, val)]) -- TODO?
                                            else m  -- Don't allow writes in 'ROM' region

emptyMem :: U8Memory
emptyMem = U8Memory (V.replicate 0x10000 0x00)


-- TODO?
memCreate :: [Word8] -> Int -> U8Memory
memCreate x len = U8Memory (V.fromList (Prelude.replicate 0x8000 0 ++ x ++ Prelude.replicate (0x8000 - len) 0) )


type CPU a b = (ExceptT B.ByteString (StateT (CPUState b) IO) a)

runCPU :: CPU a b -> CPUState b -> IO (Either B.ByteString a, CPUState b)
runCPU = runStateT . runExceptT


---------------------------------------
-- 
-- CPU register and memory manipulation
-- 
---------------------------------------

pcInc :: Memory a => (CPUState a -> b) -> Word16 -> CPUState a -> (b, CPUState a)
pcInc f i c@(CPUState _ _ _ _ _ pc _) = (f c, c { rPC = pc+i })

-- Reading from memory
-- -------------------
addrRead :: Memory a => CPUState a -> Word16 -> Word8
addrRead (CPUState _ _ _ _ _ _ mem) = readAddr mem

pcRead :: Memory a => CPUState a -> Word8
pcRead (CPUState _ _ _ _ _ pc mem) = readAddr mem pc

pcReadInc :: Memory a => CPUState a -> (Word8, CPUState a)
pcReadInc = pcInc pcRead 1


pcRead16 :: Memory a => CPUState a -> Word16
pcRead16 (CPUState _ _ _ _ _ pc mem) = asAddress (readAddr mem pc) (readAddr mem (pc+1))

pcRead16Inc :: Memory a => CPUState a -> (Word16, CPUState a)
pcRead16Inc = pcInc pcRead16 2


absRead :: Memory a => IndexRegister -> CPUState a -> Word8
absRead ir c = addrRead c (pcRead16 c + w16 (idx ir c))

absReadInc :: Memory a => IndexRegister -> CPUState a -> (Word8, CPUState a)
absReadInc ir = pcInc (absRead ir) 2

zeroPageRead :: Memory a => IndexRegister -> CPUState a -> Word8
zeroPageRead ir c = addrRead c (asAddress (pcRead c + idx ir c) 0x00)

zeroPageReadInc :: Memory a => IndexRegister -> CPUState a -> (Word8, CPUState a)
zeroPageReadInc ir = pcInc (zeroPageRead ir) 1


getIndirect :: Memory a => Word16 -> CPUState a -> Word16
getIndirect addr (CPUState _ _ _ _ _ _ mem) = asAddress (readAddr mem addr) (readAddr mem (addr + 1))

indRead :: Memory a => IndexRegister -> CPUState a -> Word8
indRead X    c@(CPUState _ _ _ _ _ _ mem) = readAddr mem (getIndirect ptr c)
    where
        ptr = asAddress (pcRead c + w8 (idx X c)) 0x00
indRead Y    c@(CPUState _ _ _ _ _ pc mem) = readAddr mem (getIndirect ptr c + w16 (idx Y c))
    where
        ptr = asAddress (readAddr mem pc) 0x00
indRead _ _ = error $ "There is no other addressing mode for indirectly accessing 8 bit numbers"

indReadInc :: Memory a => IndexRegister -> CPUState a -> (Word8, CPUState a)
indReadInc None = error $ "There is no other addressing mode for indirectly accessing 8 bit numbers"
indReadInc ir = pcInc (indRead ir) 1


indRead16 :: Memory a => IndexRegister -> CPUState a -> Word16
indRead16 None c@(CPUState _ _ _ _ _ _ mem) = asAddress (readAddr mem ptr) (readAddr mem (ptr+1))
    where
        ptr = getIndirect (pcRead16 c) c
indRead16 ir _ = error $ "There is no other addressing mode for indirectly accessing 16 bit numbers with index " ++ show ir

indRead16Inc :: Memory a => IndexRegister -> CPUState a -> (Word16, CPUState a)
indRead16Inc None = pcInc (indRead16 None) 2
indRead16Inc ir = error $ "There is no other addressing mode for indirectly accessing 16 bit numbers with index " ++ show ir


-- Writing to memory
-- -----------------
addrWrite :: Memory a => CPUState a -> Word16 -> Word8 -> CPUState a
addrWrite c@(CPUState _ _ _ _ _ _ mem) addr val = c { cMem = writeAddr mem addr val}

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

indWrite :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
indWrite None c val = addrWrite c (getIndirect (pcRead16 c) c) val -- never actually used?
indWrite X    c val = addrWrite c (getIndirect ptr newc) val
    where
        (addr, newc) = pcReadInc c
        ptr = asAddress (addr + w8 (idx X newc)) 0x00
indWrite Y    c@(CPUState _ _ _ _ _ pc mem) val = addrWrite c (getIndirect ptr c + w16 (idx Y c)) val
    where
        ptr = asAddress (readAddr mem pc) 0x00

indWriteInc :: Memory a => IndexRegister -> CPUState a -> Word8 -> CPUState a
indWriteInc ir c = addrWrite newc (asAddress (addr + idx ir c) 0x00)
    where
        (addr, newc) = pcReadInc c

-- Stack
-- | Push one byte onto the stack, decrementing the stack pointer.
pushByte :: Memory a => CPUState a -> Word8 -> CPUState a
pushByte c@(CPUState _ _ _ _ sp _ mem) val = c {rSP = sp-1, cMem = writeAddr mem (asAddress sp 0x01) val }

-- | Pull one byte from the stack, incrementing the stack pointer.
pullByte :: Memory a => CPUState a -> (Word8, CPUState a)
pullByte c@(CPUState _ _ _ _ sp _ mem) = (readAddr mem (asAddress (sp + 1) 0x01), c {rSP = sp+1})

---------------------------------------
--
-- CPU instructions
-- 
---------------------------------------

type Opcode = Word8

-- | CPU instructions
-- 
-- See [the instruction reference](https://www.nesdev.org/obelisk-6502-guide/reference.html) for
-- explanation of each individual instruction.
data Inst = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK | BVC | BVS | CLC
          | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR | INC | INX | INY | JMP
          | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL | ROR | RTI
          | RTS | SBC | SEC | SED | SEI | STA | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
          | ILL -- Illegal instruction, one of the undefined opcodes
          | STP -- Intentional stop instruction
          deriving (Show,Eq)

-- | CPU Addressing Modes
-- 
-- Different ways for accessing memory.
data AddrMode = Imp     -- Implicit
              | Acc     -- Accumulator
              | Imm     -- Immediate
              | ZP      -- Zero Page ($0000-$00ff)
              | ZPX     -- Zero Page,X
              | ZPY     -- Zero Page,Y
              | Rel     -- Relative
              | Abs     -- Absolute
              | AbsX    -- Absolute,X
              | AbsY    -- Absolute,Y
              | Ind     -- Indirect
              | IndX    -- Indexed Indirect (X)
              | IndY    -- Indexed Indirect (Y)
              deriving (Show,Eq)

-- | Index registers
-- 
-- Used in specific memory addressing modes.
data IndexRegister = None   -- No indexing
                   | X      -- X register
                   | Y      -- Y register
                    deriving (Show, Eq)

-- | Get the value at the corresponding index regsiter, or 0.
idx :: Memory a => IndexRegister -> CPUState a -> Word8
idx None _ = 0
idx X c = fromIntegral (rX c)
idx Y c = fromIntegral (rY c)


-- | Run a specific instruction on the cpu
runInst :: Memory a => Inst -> AddrMode -> CPU () a -- TODO?
runInst NOP _ = return ()

runInst BRK _ = do
    p <- gets rP
    pc <- gets rPC
    modify (`pushByte` (msb (pc)))
    modify (`pushByte` (lsb (pc)))
    modify (`pushByte` (destructP p))

    newc <- get
    let addr = getIndirect 0xfffe newc
    put $ newc {rPC = addr, rP = p {fB = True}}

runInst ADC mode = modify $ addsub id mode
runInst SBC mode = modify $ addsub (.^. 0xff) mode

runInst AND mode = modify $ bitwise (.&.) mode
runInst ORA mode = modify $ bitwise (.|.) mode
runInst EOR mode = modify $ bitwise (.^.) mode

runInst BIT mode = do
    (val,newc) <- gets (case mode of
                    ZP  -> zeroPageReadInc None
                    Abs -> absReadInc None
                    _   -> error "Unreachable")
    a <- gets rA
    p <- gets rP
    let val' = a .&. val

    let newP = p {fZ = val' == 0, fV = val `testBit` 6, fN = val `testBit` 7}
    put $ newc {rP = newP}

runInst ASL Acc = modify $ \c -> let val = rA c .<<. 1 in c {rA = val,                               rP = (rP c) {fC = rA c `testBit` 7, fZ = val == 0, fN = val `testBit` 7}}
runInst ROL Acc = modify $ \c -> let val = rA c .<<. 1 in c {rA = val .|. bToI (fC (rP c)),          rP = (rP c) {fC = rA c `testBit` 7, fZ = val == 0, fN = val `testBit` 7}}
runInst LSR Acc = modify $ \c -> let val = rA c .>>. 1 in c {rA = val,                               rP = (rP c) {fC = rA c `testBit` 0, fZ = val == 0, fN = val `testBit` 7}}
runInst ROR Acc = modify $ \c -> let val = rA c .>>. 1 in c {rA = val .|. (bToI (fC (rP c)) .<<. 7), rP = (rP c) {fC = rA c `testBit` 0, fZ = val == 0, fN = val `testBit` 7}}

runInst ASL mode = modify $ \c -> let (val,newc) = memReadWrite (.<<. 1) mode c                                         in newc {rP = (rP newc) {fC = val `testBit` 7, fZ = val == 0, fN = val `testBit` 7}}
runInst ROL mode = modify $ \c -> let (val,newc) = memReadWrite (\x -> (x .<<. 1) .|. bToI (fC (rP c))) mode c          in newc {rP = (rP newc) {fC = val `testBit` 7, fZ = val == 0, fN = val `testBit` 7}}
runInst LSR mode = modify $ \c -> let (val,newc) = memReadWrite (.>>. 1) mode c                                         in newc {rP = (rP newc) {fC = val `testBit` 0, fZ = val == 0, fN = val `testBit` 7}}
runInst ROR mode = modify $ \c -> let (val,newc) = memReadWrite (\x -> (x .>>. 1) .|. (bToI (fC (rP c)) .<<. 7)) mode c in newc {rP = (rP newc) {fC = val `testBit` 0, fZ = val == 0, fN = val `testBit` 7}}

runInst CLC _ = modify $ \c -> c {rP = (rP c) {fC = False}}
runInst CLD _ = modify $ \c -> c {rP = (rP c) {fD = False}}  -- Note: Binary Coded Decimal (BCD) is not implemented
runInst CLI _ = modify $ \c -> c {rP = (rP c) {fI = False}}
runInst CLV _ = modify $ \c -> c {rP = (rP c) {fV = False}}
runInst SEC _ = modify $ \c -> c {rP = (rP c) {fC = True}}
runInst SED _ = modify $ \c -> c {rP = (rP c) {fD = True}}   -- Note: Binary Coded Decimal (BCD) is not implemented
runInst SEI _ = modify $ \c -> c {rP = (rP c) {fI = True}}

runInst CMP mode = modify $ \c -> cmpr (rA c) mode c
runInst CPX mode = modify $ \c -> cmpr (rX c) mode c
runInst CPY mode = modify $ \c -> cmpr (rY c) mode c

runInst INC mode = modify $ \c -> let (_,newc) = memReadWrite         (+1) mode c in newc
runInst DEC mode = modify $ \c -> let (_,newc) = memReadWrite (subtract 1) mode c in newc

runInst INX _ = modify $ \c -> let val = rX c + 1 in c {rX = val, rP = (rP c) {fZ = val == 0, fN = val `testBit` 7}}
runInst DEX _ = modify $ \c -> let val = rX c - 1 in c {rX = val, rP = (rP c) {fZ = val == 0, fN = val `testBit` 7}}
runInst INY _ = modify $ \c -> let val = rY c + 1 in c {rY = val, rP = (rP c) {fZ = val == 0, fN = val `testBit` 7}}
runInst DEY _ = modify $ \c -> let val = rY c - 1 in c {rY = val, rP = (rP c) {fZ = val == 0, fN = val `testBit` 7}}

runInst BCC _ = modify $ \c -> let (val,newc) = pcReadInc c in if       fC (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BCS _ = modify $ \c -> let (val,newc) = pcReadInc c in if not $ fC (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BEQ _ = modify $ \c -> let (val,newc) = pcReadInc c in if       fZ (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BNE _ = modify $ \c -> let (val,newc) = pcReadInc c in if not $ fZ (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BMI _ = modify $ \c -> let (val,newc) = pcReadInc c in if       fN (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BPL _ = modify $ \c -> let (val,newc) = pcReadInc c in if not $ fN (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BVC _ = modify $ \c -> let (val,newc) = pcReadInc c in if       fV (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc
runInst BVS _ = modify $ \c -> let (val,newc) = pcReadInc c in if not $ fV (rP newc) then newc {rPC = rPC newc + w16 (s8 val)} else newc

runInst JMP mode = do
    (val,newc) <- gets (case mode of
                    Ind -> indRead16Inc None
                    Abs -> pcRead16Inc
                    _   -> error "Unreachable")
    put $ newc {rPC = val}
runInst JSR _ = do
    (addr, newc) <- gets pcRead16Inc
    put newc
    pc' <- gets rPC
    let pc = pc' - 1
    modify (`pushByte` (msb pc))
    modify (`pushByte` (lsb pc))
    c <- get
    put $ c {rPC = addr}
runInst RTS _ = do
    (lb, c') <- gets pullByte
    let (mb, newc) = pullByte c'
    put $ newc {rPC = (asAddress lb mb) + 1}
runInst RTI _ = do
    (p, c') <- gets pullByte
    let (lb, c'') = pullByte c'
    let (mb, newc) = pullByte c''
    put $ newc {rPC = asAddress lb mb, rP = constructP p}

runInst LDA mode = do
    (val,newc) <- gets (case mode of
                    Imm  -> pcReadInc
                    ZP   -> zeroPageReadInc None
                    ZPX  -> zeroPageReadInc X
                    Abs  -> absReadInc None
                    AbsX -> absReadInc X
                    AbsY -> absReadInc Y
                    IndX -> indReadInc X
                    IndY -> indReadInc Y
                    _    -> error "Unreachable")
    let z = val == 0
    let n = val `testBit` 7
    p <- gets rP
    let newP = p {fZ = z, fN = n}
    put $ newc {rA = val, rP = newP}
runInst LDX mode = do
    (val,newc) <- gets (case mode of
                    Imm  -> pcReadInc
                    ZP   -> zeroPageReadInc None
                    ZPY  -> zeroPageReadInc Y
                    Abs  -> absReadInc None
                    AbsY -> absReadInc Y
                    _    -> error "Unreachable")
    let z = val == 0
    let n = val `testBit` 7
    p <- gets rP
    let newP = p {fZ = z, fN = n}
    put $ newc {rX = val, rP = newP}
runInst LDY mode = do
    (val,newc) <- gets (case mode of
                    Imm  -> pcReadInc
                    ZP   -> zeroPageReadInc None
                    ZPX  -> zeroPageReadInc X
                    Abs  -> absReadInc None
                    AbsX -> absReadInc X
                    _    -> error "Unreachable")
    let z = val == 0
    let n = val `testBit` 7
    p <- gets rP
    let newP = p {fZ = z, fN = n}
    put $ newc {rY = val, rP = newP}
runInst STA mode = gets rA >>= \a -> modify $ flip (case mode of
                        ZP   -> zpWrite None
                        ZPX  -> zpWrite X
                        Abs  -> absWriteInc None
                        AbsX -> absWriteInc X
                        AbsY -> absWriteInc Y
                        IndX -> indWriteInc X
                        IndY -> indWriteInc Y
                        _    -> error "Unreachable") a
runInst STX mode = gets rX >>= \x -> modify $ flip (case mode of
                        ZP   -> zpWrite None
                        ZPY  -> zpWrite Y
                        Abs  -> absWriteInc None
                        _    -> error "Unreachable") x
runInst STY mode = gets rY >>= \y -> modify $ flip (case mode of
                        ZP   -> zpWrite None
                        ZPX  -> zpWrite X
                        Abs  -> absWriteInc None
                        _    -> error "Unreachable") y

runInst PHA _ = gets rA >>= \a -> modify $ \c -> pushByte c a
runInst PHP _ = gets rP >>= \p -> modify $ \c -> pushByte c (destructP p)
runInst PLA _ = do
    p <- gets rP
    (val,newc) <- gets pullByte
    put $ newc {rA = val, rP = p {fZ = val == 0, fN = val `testBit` 7}}
runInst PLP _ = do
    (val,newc) <- gets pullByte
    put $ newc {rP = constructP val}

runInst TAX _ = gets rP >>= \p -> gets rA  >>= \a -> modify $ \c -> c {rX  = a, rP = p {fZ = a == 0, fN = a `testBit` 7}}
runInst TAY _ = gets rP >>= \p -> gets rA  >>= \a -> modify $ \c -> c {rY  = a, rP = p {fZ = a == 0, fN = a `testBit` 7}}
runInst TSX _ = gets rP >>= \p -> gets rSP >>= \s -> modify $ \c -> c {rY  = s, rP = p {fZ = s == 0, fN = s `testBit` 7}}
runInst TXA _ = gets rP >>= \p -> gets rX  >>= \x -> modify $ \c -> c {rA  = x, rP = p {fZ = x == 0, fN = x `testBit` 7}}
runInst TXS _ = gets rP >>= \p -> gets rX  >>= \x -> modify $ \c -> c {rSP = x, rP = p {fZ = x == 0, fN = x `testBit` 7}}
runInst TYA _ = gets rP >>= \p -> gets rY  >>= \y -> modify $ \c -> c {rA  = y, rP = p {fZ = y == 0, fN = y `testBit` 7}}

runInst STP _ = throwError ""  -- not actually an error...

runInst _ _ = throwError "Cannot run illegal instruction!!!!!"


runOP :: Memory a => Opcode -> CPU () a
runOP op = runInst (opToInst op) (opToAddrMode op)

-- Extracted instructions

-- | Addition / subraction instructions (ADC, SBC)
addsub :: Memory a => (Word8 -> Word8) -> AddrMode -> CPUState a -> CPUState a
addsub f mode c = do
    let (val,newc) = (case mode of
                    Imm  -> pcReadInc
                    ZP   -> zeroPageReadInc None
                    ZPX  -> zeroPageReadInc X
                    Abs  -> absReadInc None
                    AbsX -> absReadInc X
                    AbsY -> absReadInc Y
                    IndX -> indReadInc X
                    IndY -> indReadInc Y
                    _    -> error "Unreachable") c
    let a = rA newc
    let ca = bToI $ fC $ rP newc :: Word16

    let val16 = w16 a + w16 (f val) + w16 ca
    let val' = w8 val16

    let cf = val16 > 0xff
    let z = val' == 0
    let v = (complement (a .^. val) .&. (a .^. val')) `testBit` 7
    let n = val' `testBit` 7
    let newP = (rP newc) {fC = cf, fZ = z, fV = v, fN = n}
    newc {rA = val', rP = newP}

-- | Bitwise logical instructions (AND, EOR, ORA)
bitwise :: Memory a => (Word8 -> Register8 -> Register8) -> AddrMode -> CPUState a -> CPUState a
bitwise f mode c = do
    let (val,newc) = (case mode of
                    Imm  -> pcReadInc
                    ZP   -> zeroPageReadInc None
                    ZPX  -> zeroPageReadInc X
                    Abs  -> absReadInc None
                    AbsX -> absReadInc X
                    AbsY -> absReadInc Y
                    IndX -> indReadInc X
                    IndY -> indReadInc Y
                    _    -> error "Unreachable" ) c
    let a = rA newc
    let val' = a `f` val

    let z = val' == 0
    let n = val' `testBit` 7
    let newP = (rP newc) {fZ = z, fN = n}
    newc {rA = val', rP = newP}

-- TODO?: replace these with ones that work on the CPU monad stack instead of directly on the CPUState.

-- | Comparison instructions (CMP, CPX, CPY)
cmpr :: Memory a => Word8 -> AddrMode -> CPUState a -> CPUState a
cmpr r mode c = do
    let (val,newc) = (case mode of
                    Imm  -> pcReadInc
                    ZP   -> zeroPageReadInc None
                    ZPX  -> zeroPageReadInc X       -- CMP only
                    Abs  -> absReadInc None
                    AbsX -> absReadInc X            -- CMP only
                    AbsY -> absReadInc Y            -- CMP only
                    IndX -> indReadInc X            -- CMP only
                    IndY -> indReadInc Y            -- CMP only
                    _    -> error "Unreachable" ) c
    let val' = r - val

    let cf = r >= val
    let z = r == val
    let n = val' `testBit` 7
    let newP = (rP newc) {fC = cf, fZ = z, fN = n}
    newc {rP = newP}

-- | Apply function `f` to a value memory (read v, write f v) corersponding to the addressing mode, and return the old value.
memReadWrite :: Memory a => (Word8 -> Word8) -> AddrMode -> CPUState a -> (Word8, CPUState a)
memReadWrite f mode c = do
    -- TODO: make it so you don't have to read and then write (pass func instead of val to xyzWrite(Inc) functions?)
    let val = (case mode of
                    ZP   -> zeroPageRead None
                    ZPX  -> zeroPageRead X
                    Abs  -> absRead None
                    AbsX -> absRead X
                    _   -> error "Unreachable") c
    let val' = f val
    let newc = (case mode of
                    ZP   -> zpWriteInc None c
                    ZPX  -> zpWriteInc X c
                    Abs  -> absWriteInc None c
                    AbsX -> absWriteInc X c
                    _   -> error "Unreachable") val'
    (val, newc {rP = (rP c) {fZ = val' == 0, fN = val' `testBit` 7}})



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
            | op `elem` [0xa0, 0xa4, 0xb4, 0xac, 0xbc]                   = LDY
            | op `elem` [0x4a, 0x46, 0x56, 0x4e, 0x5e]                   = LSR
            | op `elem` [0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11] = ORA
            | op `elem` [0x2a, 0x26, 0x36, 0x2e, 0x3e]                   = ROL
            | op `elem` [0x6a, 0x66, 0x76, 0x6e, 0x7e]                   = ROR
            | op `elem` [0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1] = SBC
            | op `elem` [0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91]       = STA
            | op `elem` [0x86, 0x96, 0x8e]                               = STX
            | op `elem` [0x84, 0x94, 0x8c]                               = STY
            | op `elem` [0x02, 0x12, 0x22, 0x32, 0x42, 0x62, 0x92, 0xb2, 0xd2, 0xf2] = STP
opToInst _ = ILL

-- | Get the addressing mode corresponding to the given opcode.
opToAddrMode :: Opcode -> AddrMode
opToAddrMode 0x00 = Imp
opToAddrMode 0x02 = Imp
opToAddrMode 0x10 = Rel
opToAddrMode 0x20 = Abs
opToAddrMode 0x30 = Rel
opToAddrMode 0x40 = Imp
opToAddrMode 0x50 = Rel
opToAddrMode 0x60 = Imp
opToAddrMode 0x70 = Rel
opToAddrMode 0x90 = Rel
opToAddrMode 0xb0 = Rel
opToAddrMode 0xd0 = Rel
opToAddrMode 0xf0 = Rel
opToAddrMode 0x6c = Ind
opToAddrMode 0x8a = Imp
opToAddrMode 0x9a = Imp
opToAddrMode 0xaa = Imp
opToAddrMode 0xba = Imp
opToAddrMode 0xca = Imp
opToAddrMode 0xea = Imp
opToAddrMode 0x96 = ZPY
opToAddrMode 0xb6 = ZPY
opToAddrMode 0xbe = AbsY
opToAddrMode op | (op .&. 0b11111) == 0b00000 = Imm
                | (op .&. 0b11111) == 0b00100 = ZP
                | (op .&. 0b11111) == 0b01100 = Abs
                | (op .&. 0b11111) == 0b10100 = ZPX
                | (op .&. 0b11111) == 0b11100 = AbsX

                | (op .&. 0b11111) == 0b00001 = IndX
                | (op .&. 0b11111) == 0b00101 = ZP
                | (op .&. 0b11111) == 0b01001 = Imm
                | (op .&. 0b11111) == 0b01101 = Abs
                | (op .&. 0b11111) == 0b10001 = IndY
                | (op .&. 0b11111) == 0b10101 = ZPX
                | (op .&. 0b11111) == 0b11001 = AbsY
                | (op .&. 0b11111) == 0b11101 = AbsX

                | (op .&. 0b1001_1111) == 0b1000_0010 = Imm
                | (op .&. 0b11111) == 0b00010 = Imp

                | (op .&. 0b11111) == 0b00110 = ZP
                | (op .&. 0b11111) == 0b01010 = Acc
                | (op .&. 0b11111) == 0b01110 = Abs
                | (op .&. 0b11111) == 0b10110 = ZPX
                | (op .&. 0b11111) == 0b11110 = AbsX
opToAddrMode _ = Imp