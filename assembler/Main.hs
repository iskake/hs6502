{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Applicative (show)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment
import System.FilePath

import HS6502
import HS6502.Debug
import Memory
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "usage: assembler filename" >> exitFailure
        a:_ -> putStrLn $ "Assembling file " <> a
    let filename = head args
    file <- T.readFile (filename)
    let ls = (filter (\x -> (T.head x) /= ';') -- TODO: replace with (many . pLegal) so we don't lose line numbers in error messages?
            . map T.strip
            . filter (/= "")
            . T.lines)
            file
    -- print ls
    case mapM (runParser pLegal "") ls of
        Left failures -> mapM_ (putStrLn . errorBundlePretty) [failures] >> exitFailure
        Right l -> do
            -- let regions = getRegions x -- TODO
            let labels = Map.empty -- getLabels regions x -- TODO
            let asm = map (tryAssemble labels) l
            case sequence asm of
                Left errs -> mapM_ (putStrLn . (\s -> "Assembly error: " <> T.unpack s)) [errs] >> exitFailure
                Right val -> B.writeFile (takeBaseName filename <> ".bin") (B.pack (concat val)) >> putStrLn ("Successfully assembled " <> ((takeBaseName filename) <> ".bin"))

tryAssemble :: Map Label Word16 -> SourceLine -> Either Text [Word8]
tryAssemble labels (Ins i) = assemble (labelReplace i labels)
tryAssemble _ (Bytes bs)   = Right bs
tryAssemble _ (Lab _)      = Right []
tryAssemble _ (Region _)   = Right []


data SourceLine = Ins SourceInstruction
                | Lab Label
                | Bytes [Word8]
                | Region Word16
                deriving (Show)

pLegal :: Parser SourceLine
pLegal = try (pInstruction <&> Ins)
     <|> try (pLabel <&> Lab)
     <|> try (pByte <&> Bytes)
     <|> (pRegion <&> Region)

type Parser = Parsec Void Text

type Label = Text
type Argument = Maybe (Either Word8 (Either Word16 Label))

pInst :: Parser Inst
pInst = choice
    [ ADC <$ string' "ADC"
    , AND <$ string' "AND"
    , ASL <$ string' "ASL"
    , BCC <$ string' "BCC"
    , BCS <$ string' "BCS"
    , BEQ <$ string' "BEQ"
    , BIT <$ string' "BIT"
    , BMI <$ string' "BMI"
    , BNE <$ string' "BNE"
    , BPL <$ string' "BPL"
    , BRK <$ string' "BRK"
    , BVC <$ string' "BVC"
    , BVS <$ string' "BVS"
    , CLC <$ string' "CLC"
    , CLD <$ string' "CLD"
    , CLI <$ string' "CLI"
    , CLV <$ string' "CLV"
    , CMP <$ string' "CMP"
    , CPX <$ string' "CPX"
    , CPY <$ string' "CPY"
    , DEC <$ string' "DEC"
    , DEX <$ string' "DEX"
    , DEY <$ string' "DEY"
    , EOR <$ string' "EOR"
    , INC <$ string' "INC"
    , INX <$ string' "INX"
    , INY <$ string' "INY"
    , JMP <$ string' "JMP"
    , JSR <$ string' "JSR"
    , LDA <$ string' "LDA"
    , LDX <$ string' "LDX"
    , LDY <$ string' "LDY"
    , LSR <$ string' "LSR"
    , NOP <$ string' "NOP"
    , ORA <$ string' "ORA"
    , PHA <$ string' "PHA"
    , PHP <$ string' "PHP"
    , PLA <$ string' "PLA"
    , PLP <$ string' "PLP"
    , ROL <$ string' "ROL"
    , ROR <$ string' "ROR"
    , RTI <$ string' "RTI"
    , RTS <$ string' "RTS"
    , SBC <$ string' "SBC"
    , SEC <$ string' "SEC"
    , SED <$ string' "SED"
    , SEI <$ string' "SEI"
    , STA <$ string' "STA"
    , STX <$ string' "STX"
    , STY <$ string' "STY"
    , TAX <$ string' "TAX"
    , TAY <$ string' "TAY"
    , TSX <$ string' "TSX"
    , TXA <$ string' "TXA"
    , TXS <$ string' "TXS"
    , TYA <$ string' "TYA"
    , ILL <$ string' "ILL" ]

pLabel :: Parser Label
pLabel = do
    -- void (char '.')
    s <- T.pack <$> some letterChar
    void (char ':')
    return s

pByte :: Parser [Word8]
pByte = do
    void (string' ".db")
    sc
    many pWord8

pRegion :: Parser Word16
pRegion = do
    void (string' ".region")
    sc
    pWord16

pAddrMode :: Parser (AddrMode, Argument)
pAddrMode = try imm  <|>
            try zpx  <|>
            try zpy  <|>
            try absx <|>
            try absy <|>
            try ind  <|>
            try indx <|>
            try indy <|>
            try absn <|>
            try rel  <|>
            try zp   <|>
            try imp  <|>
            acc
    where
        imm  = (void (char '#') *> (pWord8 <* notFollowedBy (char ','))) >>= \x -> return (Imm, Just (Left x))
        zpx  = (pWord8) >>= \x -> void (string' ",x") >> return (ZPX, Just (Left x))
        zpy  = (pWord8) >>= \x -> void (string' ",y") >> return (ZPY, Just (Left x))
        absx = (pWord16OrLabel) >>= \x -> void (string' ",x") >> return (AbsX, Just (Right x))
        absy = (pWord16OrLabel) >>= \x -> void (string' ",y") >> return (AbsY, Just (Right x))
        absn = (pWord16OrLabel) >>= \x -> return (Abs, Just (Right x))
        ind  = void (char '[') >> (pWord16OrLabel) >>= \x -> void (char ']') >> return (Ind, Just (Right x))
        indx  = void (char '[') >> (pWord8) >>= \x -> void (string' ",x") >> (char ']') >> return (IndX, Just (Left x))
        indy = void (char '[') >> (pWord8) >>= \x -> (char ']') >> void (string' ",y") >> return (IndY, Just (Left x))
        zp   = (pWord8) >>= \x -> return (ZP,  Just (Left x))
        rel  = (pWord8) >>= \x -> return (Rel, Just (Left x))
        imp  = return (Imp, Nothing)
        acc  = return (Acc, Nothing)

pWord8 :: Parser Word8
pWord8 = do
    void (char '$')
    _ <- lookAhead ((hexDigitChar >> hexDigitChar) <* notFollowedBy hexDigitChar)
    lexeme L.hexadecimal

pWord16 :: Parser Word16
pWord16 = do
    void (char '$')
    _ <- lookAhead ((hexDigitChar >> hexDigitChar >> hexDigitChar >> hexDigitChar) <* notFollowedBy hexDigitChar)
    lexeme L.hexadecimal

pWord16OrLabel :: Parser (Either Word16 Label)
pWord16OrLabel = try ((T.pack <$> some letterChar) <&> Right)
             <|> (pWord16 <&> Left)

sc0 :: Parser ()
sc0 = L.space space (L.skipLineComment ";") empty

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type SourceInstruction = (Inst, AddrMode, Argument)

pInstruction :: Parser SourceInstruction
pInstruction = do
    inst <- pInst <?> "valid instruction"
    sc
    (amode, arg) <- pAddrMode
    sc
    return (inst, amode, arg)

labeler :: Argument -> Map Label Word16 -> Maybe (Either Word8 Word16)
labeler (Just (Right (Right l))) m = Map.lookup l m >>= Just . Right
labeler (Just (Right (Left x)))  _ = Just (Right x)
labeler (Just (Left x))          _ = Just (Left x)
labeler Nothing                  _ = Nothing

labelReplace :: SourceInstruction -> Map Label Word16 -> Instruction
labelReplace (i,am,a) m = Instruction i am (labeler a m)

-- The quick and easy way to do it
assemble :: Instruction -> Either Text [Word8]
assemble (Instruction BRK Imp  Nothing)             = Right $ [0x00]
assemble (Instruction ORA IndX (Just (Right val)))  = Right $ [0x01] <> (fromAddress val)
assemble (Instruction ORA ZP   (Just (Left val)))   = Right $ [0x05, val]
assemble (Instruction ASL ZP   (Just (Left val)))   = Right $ [0x06, val]
assemble (Instruction PHP Imp  Nothing)             = Right $ [0x08]
assemble (Instruction ORA Imm  (Just (Left val)))   = Right $ [0x09, val]
assemble (Instruction ASL Acc  Nothing)             = Right $ [0x0a]
assemble (Instruction ORA Abs  (Just (Right val)))  = Right $ [0x0d] <> (fromAddress val)
assemble (Instruction ASL Abs  (Just (Right val)))  = Right $ [0x0e] <> (fromAddress val)
assemble (Instruction BPL Rel  (Just (Left val)))   = Right $ [0x10, val] -- TODO: adjust?
assemble (Instruction ORA IndY (Just (Right val)))  = Right $ [0x11] <> (fromAddress val)
assemble (Instruction ORA ZPX  (Just (Left val)))   = Right $ [0x15, val]
assemble (Instruction ASL ZPX  (Just (Left val)))   = Right $ [0x16, val]
assemble (Instruction CLC Imp  Nothing)             = Right $ [0x18]
assemble (Instruction ORA AbsY (Just (Right val)))  = Right $ [0x19] <> (fromAddress val)
assemble (Instruction ORA AbsX (Just (Right val)))  = Right $ [0x1d] <> (fromAddress val)
assemble (Instruction ASL AbsX (Just (Right val)))  = Right $ [0x1e] <> (fromAddress val)
assemble (Instruction JSR Abs  (Just (Right val)))  = Right $ [0x20] <> (fromAddress val)
assemble (Instruction AND IndX (Just (Right val)))  = Right $ [0x21] <> (fromAddress val)
assemble (Instruction BIT ZP   (Just (Left val)))   = Right $ [0x24, val]
assemble (Instruction AND ZP   (Just (Left val)))   = Right $ [0x25, val]
assemble (Instruction ROL ZP   (Just (Left val)))   = Right $ [0x26, val]
assemble (Instruction PLP Imp  Nothing)             = Right $ [0x28]
assemble (Instruction AND Imm  (Just (Left val)))   = Right $ [0x29, val]
assemble (Instruction ROL Acc  Nothing)             = Right $ [0x2a]
assemble (Instruction BIT Abs  (Just (Right val)))  = Right $ [0x2c] <> (fromAddress val)
assemble (Instruction AND Abs  (Just (Right val)))  = Right $ [0x2d] <> (fromAddress val)
assemble (Instruction ROL Abs  (Just (Right val)))  = Right $ [0x2e] <> (fromAddress val)
assemble (Instruction BMI Rel  (Just (Left val)))   = Right $ [0x30, val] -- TODO: adjust?
assemble (Instruction AND IndY (Just (Right val)))  = Right $ [0x31] <> (fromAddress val)
assemble (Instruction AND ZPX  (Just (Left val)))   = Right $ [0x35, val]
assemble (Instruction ROL ZPX  (Just (Left val)))   = Right $ [0x36, val]
assemble (Instruction SEC Imp  Nothing)             = Right $ [0x38]
assemble (Instruction AND AbsY (Just (Right val)))  = Right $ [0x39] <> (fromAddress val)
assemble (Instruction AND AbsX (Just (Right val)))  = Right $ [0x3d] <> (fromAddress val)
assemble (Instruction ROL AbsX (Just (Right val)))  = Right $ [0x3e] <> (fromAddress val)
assemble (Instruction RTI Imp  Nothing)             = Right $ [0x40]
assemble (Instruction EOR IndX (Just (Right val)))  = Right $ [0x41] <> (fromAddress val)
assemble (Instruction EOR ZP   (Just (Left val)))   = Right $ [0x45, val]
assemble (Instruction LSR ZP   (Just (Left val)))   = Right $ [0x46, val]
assemble (Instruction PHA Imp  Nothing)             = Right $ [0x48]
assemble (Instruction EOR Imm  (Just (Left val)))   = Right $ [0x49, val]
assemble (Instruction LSR Acc  Nothing)             = Right $ [0x4a]
assemble (Instruction JMP Abs  (Just (Right val)))  = Right $ [0x4c] <> (fromAddress val)
assemble (Instruction EOR Abs  (Just (Right val)))  = Right $ [0x4d] <> (fromAddress val)
assemble (Instruction LSR Abs  (Just (Right val)))  = Right $ [0x4e] <> (fromAddress val)
assemble (Instruction BVC Rel  (Just (Left val)))   = Right $ [0x50, val] -- TODO: adjust?
assemble (Instruction EOR IndY (Just (Right val)))  = Right $ [0x51] <> (fromAddress val)
assemble (Instruction EOR ZPX  (Just (Left val)))   = Right $ [0x55, val]
assemble (Instruction LSR ZPX  (Just (Left val)))   = Right $ [0x56, val]
assemble (Instruction CLI Imp  Nothing)             = Right $ [0x58]
assemble (Instruction EOR AbsY (Just (Right val)))  = Right $ [0x59] <> (fromAddress val)
assemble (Instruction EOR AbsX (Just (Right val)))  = Right $ [0x5d] <> (fromAddress val)
assemble (Instruction LSR AbsX (Just (Right val)))  = Right $ [0x5e] <> (fromAddress val)
assemble (Instruction RTS Imp  Nothing)             = Right $ [0x60]
assemble (Instruction ADC IndX (Just (Right val)))  = Right $ [0x61] <> (fromAddress val)
assemble (Instruction ADC ZP   (Just (Left val)))   = Right $ [0x65, val]
assemble (Instruction ROR ZP   (Just (Left val)))   = Right $ [0x66, val]
assemble (Instruction PLA Imp  Nothing)             = Right $ [0x68]
assemble (Instruction ADC Imm  (Just (Left val)))   = Right $ [0x69, val]
assemble (Instruction ROR Acc  Nothing)             = Right $ [0x6a]
assemble (Instruction JMP Ind  (Just (Right val)))  = Right $ [0x6c] <> (fromAddress val)
assemble (Instruction ADC Abs  (Just (Right val)))  = Right $ [0x6d] <> (fromAddress val)
assemble (Instruction ROR Abs  (Just (Right val)))  = Right $ [0x6e] <> (fromAddress val)
assemble (Instruction BVS Rel  (Just (Left val)))   = Right $ [0x70, val] -- TODO: adjust?
assemble (Instruction ADC IndY (Just (Right val)))  = Right $ [0x71] <> (fromAddress val)
assemble (Instruction ADC ZPX  (Just (Left val)))   = Right $ [0x75, val]
assemble (Instruction ROR ZPX  (Just (Left val)))   = Right $ [0x76, val]
assemble (Instruction SEI Imp  Nothing)             = Right $ [0x78]
assemble (Instruction ADC AbsY (Just (Right val)))  = Right $ [0x79] <> (fromAddress val)
assemble (Instruction ADC AbsX (Just (Right val)))  = Right $ [0x7d] <> (fromAddress val)
assemble (Instruction ROR AbsX (Just (Right val)))  = Right $ [0x7e] <> (fromAddress val)
assemble (Instruction STA IndX (Just (Right val)))  = Right $ [0x81] <> (fromAddress val)
assemble (Instruction STY ZP   (Just (Left val)))   = Right $ [0x84, val]
assemble (Instruction STA ZP   (Just (Left val)))   = Right $ [0x85, val]
assemble (Instruction STX ZP   (Just (Left val)))   = Right $ [0x86, val]
assemble (Instruction DEY Imp  Nothing)             = Right $ [0x88]
assemble (Instruction TXA Imp  Nothing)             = Right $ [0x8a]
assemble (Instruction STY Abs  (Just (Right val)))  = Right $ [0x8c] <> (fromAddress val)
assemble (Instruction STA Abs  (Just (Right val)))  = Right $ [0x8d] <> (fromAddress val)
assemble (Instruction STX Abs  (Just (Right val)))  = Right $ [0x8e] <> (fromAddress val)
assemble (Instruction BCC Rel  (Just (Left val)))   = Right $ [0x90, val] -- TODO: adjust?
assemble (Instruction STA IndY (Just (Right val)))  = Right $ [0x91] <> (fromAddress val)
assemble (Instruction STY ZPX  (Just (Left val)))   = Right $ [0x94, val]
assemble (Instruction STA ZPX  (Just (Left val)))   = Right $ [0x95, val]
assemble (Instruction STX ZPY  (Just (Left val)))   = Right $ [0x96, val]
assemble (Instruction TYA Imp  Nothing)             = Right $ [0x98]
assemble (Instruction STA AbsY (Just (Right val)))  = Right $ [0x99] <> (fromAddress val)
assemble (Instruction TXS Imp  Nothing)             = Right $ [0x9a]
assemble (Instruction STA AbsX (Just (Right val)))  = Right $ [0x9d] <> (fromAddress val)
assemble (Instruction LDY Imm  (Just (Left val)))   = Right $ [0xa0, val]
assemble (Instruction LDA IndX (Just (Right val)))  = Right $ [0xa1] <> (fromAddress val)
assemble (Instruction LDX Imm  (Just (Left val)))   = Right $ [0xa2, val]
assemble (Instruction LDY ZP   (Just (Left val)))   = Right $ [0xa4, val]
assemble (Instruction LDA ZP   (Just (Left val)))   = Right $ [0xa5, val]
assemble (Instruction LDX ZP   (Just (Left val)))   = Right $ [0xa6, val]
assemble (Instruction TAY Imp  Nothing)             = Right $ [0xa8]
assemble (Instruction LDA Imm  (Just (Left val)))   = Right $ [0xa9, val]
assemble (Instruction TAX Imp  Nothing)             = Right $ [0xaa]
assemble (Instruction LDY Abs  (Just (Right val)))  = Right $ [0xac] <> (fromAddress val)
assemble (Instruction LDA Abs  (Just (Right val)))  = Right $ [0xad] <> (fromAddress val)
assemble (Instruction LDX Abs  (Just (Right val)))  = Right $ [0xae] <> (fromAddress val)
assemble (Instruction BCS Rel  (Just (Left val)))   = Right $ [0xb0, val] -- TODO: adjust?
assemble (Instruction LDA IndY (Just (Right val)))  = Right $ [0xb1] <> (fromAddress val)
assemble (Instruction LDY ZPX  (Just (Left val)))   = Right $ [0xb4, val]
assemble (Instruction LDA ZPX  (Just (Left val)))   = Right $ [0xb5, val]
assemble (Instruction LDX ZPY  (Just (Left val)))   = Right $ [0xb6, val]
assemble (Instruction CLV Imp  Nothing)             = Right $ [0xb8]
assemble (Instruction LDA AbsY (Just (Right val)))  = Right $ [0xb9] <> (fromAddress val)
assemble (Instruction TSX Imp  Nothing)             = Right $ [0xba]
assemble (Instruction LDY AbsX (Just (Right val)))  = Right $ [0xbc] <> (fromAddress val)
assemble (Instruction LDA AbsX (Just (Right val)))  = Right $ [0xbd] <> (fromAddress val)
assemble (Instruction LDX AbsY (Just (Right val)))  = Right $ [0xbe] <> (fromAddress val)
assemble (Instruction CPY Imm  (Just (Left val)))   = Right $ [0xc0, val]
assemble (Instruction CMP IndX (Just (Right val)))  = Right $ [0xc1] <> (fromAddress val)
assemble (Instruction CPY ZP   (Just (Left val)))   = Right $ [0xc4, val]
assemble (Instruction CMP ZP   (Just (Left val)))   = Right $ [0xc5, val]
assemble (Instruction DEC ZP   (Just (Left val)))   = Right $ [0xc6, val]
assemble (Instruction INY Imp  Nothing)             = Right $ [0xc8]
assemble (Instruction CMP Imm  (Just (Left val)))   = Right $ [0xc9, val]
assemble (Instruction DEX Imp  Nothing)             = Right $ [0xca]
assemble (Instruction CPY Abs  (Just (Right val)))  = Right $ [0xcc] <> (fromAddress val)
assemble (Instruction CMP Abs  (Just (Right val)))  = Right $ [0xcd] <> (fromAddress val)
assemble (Instruction DEC Abs  (Just (Right val)))  = Right $ [0xce] <> (fromAddress val)
assemble (Instruction BNE Rel  (Just (Left val)))   = Right $ [0xd0, val] -- TODO: adjust?
assemble (Instruction CMP IndY (Just (Right val)))  = Right $ [0xd1] <> (fromAddress val)
assemble (Instruction CMP ZPX  (Just (Left val)))   = Right $ [0xd5, val]
assemble (Instruction DEC ZPX  (Just (Left val)))   = Right $ [0xd6, val]
assemble (Instruction CLD Imp  Nothing)             = Right $ [0xd8]
assemble (Instruction CMP AbsY (Just (Right val)))  = Right $ [0xd9] <> (fromAddress val)
assemble (Instruction CMP AbsX (Just (Right val)))  = Right $ [0xdd] <> (fromAddress val)
assemble (Instruction DEC AbsX (Just (Right val)))  = Right $ [0xde] <> (fromAddress val)
assemble (Instruction CPX Imm  (Just (Left val)))   = Right $ [0xe0, val]
assemble (Instruction SBC IndX (Just (Right val)))  = Right $ [0xe1] <> (fromAddress val)
assemble (Instruction CPX ZP   (Just (Left val)))   = Right $ [0xe4, val]
assemble (Instruction SBC ZP   (Just (Left val)))   = Right $ [0xe5, val]
assemble (Instruction INC ZP   (Just (Left val)))   = Right $ [0xe6, val]
assemble (Instruction INX Imp  Nothing)             = Right $ [0xe8]
assemble (Instruction SBC Imm  (Just (Left val)))   = Right $ [0xe9, val]
assemble (Instruction NOP Imp  Nothing)             = Right $ [0xea]
assemble (Instruction CPX Abs  (Just (Right val)))  = Right $ [0xec] <> (fromAddress val)
assemble (Instruction SBC Abs  (Just (Right val)))  = Right $ [0xed] <> (fromAddress val)
assemble (Instruction INC Abs  (Just (Right val)))  = Right $ [0xee] <> (fromAddress val)
assemble (Instruction BEQ Rel  (Just (Left val)))   = Right $ [0xf0, val] -- TODO: adjust?
assemble (Instruction SBC IndY (Just (Right val)))  = Right $ [0xf1] <> (fromAddress val)
assemble (Instruction SBC ZPX  (Just (Left val)))   = Right $ [0xf5, val]
assemble (Instruction INC ZPX  (Just (Left val)))   = Right $ [0xf6, val]
assemble (Instruction SED Imp  Nothing)             = Right $ [0xf8]
assemble (Instruction SBC AbsY (Just (Right val)))  = Right $ [0xf9] <> (fromAddress val)
assemble (Instruction SBC AbsX (Just (Right val)))  = Right $ [0xfd] <> (fromAddress val)
assemble (Instruction INC AbsX (Just (Right val)))  = Right $ [0xfe] <> (fromAddress val)
assemble (Instruction ILL _ _) = Left ("cannot assemble illegal instruction")
assemble (Instruction i m a) = do
    let ar = case a of
            Nothing  -> "no"
            (Just b) -> case b of
                (Left  w) -> "$" <> (T.pack (hex8 w)) <> " as"
                (Right w) -> "$" <> (T.pack (hex16 w)) <> " as"
    Left ("instruction " <> T.pack (show i) <> " with addressing mode " <> T.pack (show m) <> " and " <> ar <> " argument")
