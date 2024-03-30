module HS6502 where
import Data.Word
import Control.Monad.State
import Control.Monad.Except
import Data.Vector hiding ((++),modify)


-- CPU needs:
-- registers:
-- - A, X, Y, P, SP :: Word8
-- - PC  :: Word16
type Register8 = Word8
type Register16 = Word16

data CPUState = CPUState { rA  :: Register8 -- Accumulator
                         , rX  :: Register8 -- X index register
                         , rY  :: Register8 -- Y index register
                         , rP  :: Register8 -- Processor status
                         , rSP :: Register8 -- Stack pointer
                         , rPC :: Register16 -- Program counter
                         , cMem :: Memory
                         }

type Opcode = Word8

type Memory = Vector Word8

type CPU' a = ExceptT (String) (StateT CPUState IO) a
type CPU = CPU' ()

-- data CPUMem = CPUMem CPU Memory


-- stepCPU :: CPU -> Memory -> CPU
stepCPU = runStateT . runExceptT

-- General fuctions for handling CPU operations
-- getCPUState :: CPU -> CPUState
-- getCPUState cpu = do
--     s' <- gets

setA :: Word8 -> CPUState -> CPUState
setA val c = CPUState val
                      (rX  c)
                      (rY  c)
                      (rP  c)
                      (rSP c)
                      (rPC c)
                      (cMem c)

setX :: Word8 -> CPUState -> CPUState
setX val c = CPUState (rA  c)
                      val
                      (rY  c)
                      (rP  c)
                      (rSP c)
                      (rPC c)
                      (cMem c)

setY :: Word8 -> CPUState -> CPUState
setY val c = CPUState (rA  c)
                      (rX  c)
                      val
                      (rP  c)
                      (rSP c)
                      (rPC c)
                      (cMem c)

setP :: Word8 -> CPUState -> CPUState
setP val c = CPUState (rA  c)
                      (rX  c)
                      (rY  c)
                      val
                      (rSP c)
                      (rPC c)
                      (cMem c)

setPC :: Word16 -> CPUState -> CPUState
setPC val c = CPUState (rA  c)
                      (rX  c)
                      (rY  c)
                      (rP  c)
                      (rSP c)
                      val
                      (cMem c)

-- ...

-- TODO: Replace with MVector??
readMemAddr :: Word16 -> Memory -> Word8
readMemAddr addr mem = mem ! fromIntegral addr

pcRead :: CPUState -> Word8
pcRead (CPUState _ _ _ _ _ pc mem) = readMemAddr pc mem

pcReadInc :: CPUState -> (Word8, CPUState)
pcReadInc c@(CPUState _ _ _ _ _ pc mem) = (readMemAddr pc mem, setPC (pc+1) c)


-- CPU instructions:
-- -------------------
-- |ADC|AND|ASL|BCC|BCS|BEQ|BIT|BMI|BNE|BPL|BRK|BVC|BVS|CLC|
-- |CLD|CLI|CLV|CMP|CPX|CPY|DEC|DEX|DEY|EOR|INC|INX|INY|JMP|
-- |JSR|LDA|LDX|LDY|LSR|NOP|ORA|PHA|PHP|PLA|PLP|ROL|ROR|RTI|
-- |RTS|SBC|SEC|SED|SEI|STA|STX|STY|TAX|TAY|TSX|TXA|TXS|TYA|

-- adc :: CPUMem -> Opcode -> CPUMem
adc cpu 0x69 = undefined    -- immediate
adc cpu 0x65 = undefined    -- zero page
adc cpu 0x75 = undefined    -- zero page,x
adc cpu 0x6d = undefined    -- absolute
adc cpu 0x7d = undefined    -- absolute,x
adc cpu 0x79 = undefined    -- absolute,y
adc cpu 0x61 = undefined    -- (indirect,x)
adc cpu 0x71 = undefined    -- (indirect),y
adc cpu op = error $ "Undefined ADC opcode: " ++ show op

and :: CPU -> Opcode -> CPU
and cpu 0x29 = undefined    -- immediate
and cpu 0x25 = undefined    -- zero page
and cpu 0x35 = undefined    -- zero page,x
and cpu 0x2d = undefined    -- absolute
and cpu 0x3d = undefined    -- absolute,x
and cpu 0x39 = undefined    -- absolute,y
and cpu 0x21 = undefined    -- (indirect,x)
and cpu 0x31 = undefined    -- (indirect),y
and cpu op = error $ "Undefined AND opcode: " ++ show op

asl :: CPU -> Opcode -> CPU
asl cpu 0x0a = undefined  -- acc
asl cpu 0x06 = undefined  -- zp
asl cpu 0x16 = undefined  -- zp,x
asl cpu 0x0e = undefined  -- abs
asl cpu 0x1e = undefined  -- abs,x
asl cpu op = error $ "Undefined ASL opcode: " ++ show op

bcc :: CPU -> Opcode -> CPU
bcc cpu _ = undefined

bcs :: CPU -> Opcode -> CPU
bcs cpu _ = undefined

beq :: CPU -> Opcode -> CPU
beq cpu _ = undefined

lda :: Opcode -> CPU' CPUState
lda op = do   -- immediate
    c <- get
    let (val,newc) = case op of
            0xa9 -> pcReadInc c    -- immediate
            0xa5 -> undefined -- zero page
            0xb5 -> undefined -- zero page,x
            0xad -> undefined -- absolute
            0xbd -> undefined -- absolute,x
            0xb9 -> undefined -- absolute,y
            0xa1 -> undefined -- (indirect,x)
            0xb1 -> undefined -- (indirect),y
            _ -> error $ "Undefined LDA opcode: " ++ show op
    return (setP (0) (setA val newc))
-- lda 0xa5 = undefined    -- zero page
-- lda 0xb5 = undefined    -- zero page,x
-- lda 0xad = undefined    -- absolute
-- lda 0xbd = undefined    -- absolute,x
-- lda 0xb9 = undefined    -- absolute,y
-- lda 0xa1 = undefined    -- (indirect,x)
-- lda 0xb1 = undefined    -- (indirect),y
-- lda op = error $ "Undefined LDA opcode: " ++ show op

-- ldx :: CPUMem -> Opcode -> CPUMem
ldx cpu 0xa2 = undefined   -- immediate
ldx cpu 0xa6 = undefined   -- zero page
ldx cpu 0xb6 = undefined   -- zero page,y
ldx cpu 0xae = undefined   -- absolute
ldx cpu 0xbe = undefined   -- absolute,y
ldx cpu op = error $ "Undefined LDX opcode: " ++ show op

-- ldy :: CPUMem -> Opcode -> CPUMem
ldy cpu 0xa0 = undefined   -- immediate
ldy cpu 0xa4 = undefined   -- zero page
ldy cpu 0xb4 = undefined   -- zero page,x
ldy cpu 0xac = undefined   -- absolute
ldy cpu 0xbc = undefined   -- absolute,x
ldy cpu op = error $ "Undefined LDY opcode: " ++ show op

sta :: CPU -> Opcode -> CPU
sta cpu 0x85 = undefined    -- zero page
sta cpu 0x95 = undefined    -- zero page,x
sta cpu 0x8d = undefined    -- absolute
sta cpu 0x9d = undefined    -- absolute,x
sta cpu 0x99 = undefined    -- absolute,y
sta cpu 0x81 = undefined    -- (indirect,x)
sta cpu 0x91 = undefined    -- (indirect),y
sta cpu op = error $ "Undefined LDA opcode: " ++ show op

-- do nothing. 2 cycles
nop :: CPU -> Opcode -> CPU
nop cpu _ = cpu