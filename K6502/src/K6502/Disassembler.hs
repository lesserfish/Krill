module K6502.Disassembler (
    disassembleL,
    disassembleL',
    disassembleM,
    disassembleM',
) where

import Utils
import K6502.Types
import Data.Word
import Data.Int
import Text.Printf
import qualified Data.Map as Map

opInfo :: Word8 -> Maybe (String, ADDR_MODE)
opInfo 0x69 = Just ("ADC", IMMEDIATE)
opInfo 0x65 = Just ("ADC", ZEROPAGE)
opInfo 0x75 = Just ("ADC", ZEROPAGE_X)
opInfo 0x6D = Just ("ADC", ABSOLUTE)
opInfo 0x7D = Just ("ADC", ABSOLUTE_X)
opInfo 0x79 = Just ("ADC", ABSOLUTE_Y)
opInfo 0x61 = Just ("ADC", INDIRECT_X)
opInfo 0x71 = Just ("ADC", INDIRECT_Y)
opInfo 0x29 = Just ("AND", IMMEDIATE)
opInfo 0x25 = Just ("AND", ZEROPAGE)
opInfo 0x35 = Just ("AND", ZEROPAGE_X)
opInfo 0x2D = Just ("AND", ABSOLUTE)
opInfo 0x3D = Just ("AND", ABSOLUTE_X)
opInfo 0x39 = Just ("AND", ABSOLUTE_Y)
opInfo 0x21 = Just ("AND", INDIRECT_X)
opInfo 0x31 = Just ("AND", INDIRECT_Y)
opInfo 0x0A = Just ("ASL", ACCUMULATOR)
opInfo 0x06 = Just ("ASL", ZEROPAGE)
opInfo 0x16 = Just ("ASL", ZEROPAGE_X)
opInfo 0x0E = Just ("ASL", ABSOLUTE)
opInfo 0x1E = Just ("ASL", ABSOLUTE_X)
opInfo 0x90 = Just ("BCC", RELATIVE)
opInfo 0xB0 = Just ("BCS", RELATIVE)
opInfo 0xF0 = Just ("BEQ", RELATIVE)
opInfo 0x24 = Just ("BIT", ZEROPAGE)
opInfo 0x2C = Just ("BIT", ABSOLUTE)
opInfo 0x30 = Just ("BMI", RELATIVE)
opInfo 0xD0 = Just ("BNE", RELATIVE)
opInfo 0x10 = Just ("BPL", RELATIVE)
opInfo 0x00 = Just ("BRK", IMPLICIT)
opInfo 0x50 = Just ("BVC", RELATIVE)
opInfo 0x70 = Just ("BVS", RELATIVE)
opInfo 0x18 = Just ("CLC", IMPLICIT)
opInfo 0xD8 = Just ("CLD", IMPLICIT)
opInfo 0x58 = Just ("CLI", IMPLICIT)
opInfo 0xB8 = Just ("CLV", IMPLICIT)
opInfo 0xC9 = Just ("CMP", IMMEDIATE)
opInfo 0xC5 = Just ("CMP", ZEROPAGE)
opInfo 0xD5 = Just ("CMP", ZEROPAGE_X)
opInfo 0xCD = Just ("CMP", ABSOLUTE)
opInfo 0xDD = Just ("CMP", ABSOLUTE_X)
opInfo 0xD9 = Just ("CMP", ABSOLUTE_Y)
opInfo 0xC1 = Just ("CMP", INDIRECT_X)
opInfo 0xD1 = Just ("CMP", INDIRECT_Y)
opInfo 0xE0 = Just ("CPX", IMMEDIATE)
opInfo 0xE4 = Just ("CPX", ZEROPAGE)
opInfo 0xEC = Just ("CPX", ABSOLUTE)
opInfo 0xC0 = Just ("CPY", IMMEDIATE)
opInfo 0xC4 = Just ("CPY", ZEROPAGE)
opInfo 0xCC = Just ("CPY", ABSOLUTE)
opInfo 0xC6 = Just ("DEC", ZEROPAGE)
opInfo 0xD6 = Just ("DEC", ZEROPAGE_X)
opInfo 0xCE = Just ("DEC", ABSOLUTE)
opInfo 0xDE = Just ("DEC", ABSOLUTE_X)
opInfo 0xCA = Just ("DEX", IMPLICIT)
opInfo 0x88 = Just ("DEY", IMPLICIT)
opInfo 0x49 = Just ("EOR", IMMEDIATE)
opInfo 0x45 = Just ("EOR", ZEROPAGE)
opInfo 0x55 = Just ("EOR", ZEROPAGE_X)
opInfo 0x4D = Just ("EOR", ABSOLUTE)
opInfo 0x5D = Just ("EOR", ABSOLUTE_X)
opInfo 0x59 = Just ("EOR", ABSOLUTE_Y)
opInfo 0x41 = Just ("EOR", INDIRECT_X)
opInfo 0x51 = Just ("EOR", INDIRECT_Y)
opInfo 0xE6 = Just ("INC", ZEROPAGE)
opInfo 0xF6 = Just ("INC", ZEROPAGE_X)
opInfo 0xEE = Just ("INC", ABSOLUTE)
opInfo 0xFE = Just ("INC", ABSOLUTE_X)
opInfo 0xE8 = Just ("INX", IMPLICIT)
opInfo 0xC8 = Just ("INY", IMPLICIT)
opInfo 0x4C = Just ("JMP", ABSOLUTE)
opInfo 0x6C = Just ("JMP", INDIRECT)
opInfo 0x20 = Just ("JSR", ABSOLUTE)
opInfo 0xA9 = Just ("LDA", IMMEDIATE)
opInfo 0xA5 = Just ("LDA", ZEROPAGE)
opInfo 0xB5 = Just ("LDA", ZEROPAGE_X)
opInfo 0xAD = Just ("LDA", ABSOLUTE)
opInfo 0xBD = Just ("LDA", ABSOLUTE_X)
opInfo 0xB9 = Just ("LDA", ABSOLUTE_Y)
opInfo 0xA1 = Just ("LDA", INDIRECT_X)
opInfo 0xB1 = Just ("LDA", INDIRECT_Y)
opInfo 0xA2 = Just ("LDX", IMMEDIATE)
opInfo 0xA6 = Just ("LDX", ZEROPAGE)
opInfo 0xB6 = Just ("LDX", ZEROPAGE_Y)
opInfo 0xAE = Just ("LDX", ABSOLUTE)
opInfo 0xBE = Just ("LDX", ABSOLUTE_Y)
opInfo 0xA0 = Just ("LDY", IMMEDIATE)
opInfo 0xA4 = Just ("LDY", ZEROPAGE)
opInfo 0xB4 = Just ("LDY", ZEROPAGE_X)
opInfo 0xAC = Just ("LDY", ABSOLUTE)
opInfo 0xBC = Just ("LDY", ABSOLUTE_X)
opInfo 0x4A = Just ("LSR", ACCUMULATOR)
opInfo 0x46 = Just ("LSR", ZEROPAGE)
opInfo 0x56 = Just ("LSR", ZEROPAGE_X)
opInfo 0x4E = Just ("LSR", ABSOLUTE)
opInfo 0x5E = Just ("LSR", ABSOLUTE_X)
opInfo 0xEA = Just ("NOP", IMPLICIT)
opInfo 0x09 = Just ("ORA", IMMEDIATE)
opInfo 0x05 = Just ("ORA", ZEROPAGE)
opInfo 0x15 = Just ("ORA", ZEROPAGE_X)
opInfo 0x0D = Just ("ORA", ABSOLUTE)
opInfo 0x1D = Just ("ORA", ABSOLUTE_X)
opInfo 0x19 = Just ("ORA", ABSOLUTE_Y)
opInfo 0x01 = Just ("ORA", INDIRECT_X)
opInfo 0x11 = Just ("ORA", INDIRECT_Y)
opInfo 0x48 = Just ("PHA", IMPLICIT)
opInfo 0x08 = Just ("PHP", IMPLICIT)
opInfo 0x68 = Just ("PLA", IMPLICIT)
opInfo 0x28 = Just ("PLP", IMPLICIT)
opInfo 0x2A = Just ("ROL", ACCUMULATOR)
opInfo 0x26 = Just ("ROL", ZEROPAGE)
opInfo 0x36 = Just ("ROL", ZEROPAGE_X)
opInfo 0x2E = Just ("ROL", ABSOLUTE)
opInfo 0x3E = Just ("ROL", ABSOLUTE_X)
opInfo 0x6A = Just ("ROR", ACCUMULATOR)
opInfo 0x66 = Just ("ROR", ZEROPAGE)
opInfo 0x76 = Just ("ROR", ZEROPAGE_X)
opInfo 0x6E = Just ("ROR", ABSOLUTE)
opInfo 0x7E = Just ("ROR", ABSOLUTE_X)
opInfo 0x40 = Just ("RTI", IMPLICIT)
opInfo 0x60 = Just ("RTS", IMPLICIT)
opInfo 0xE9 = Just ("SBC", IMMEDIATE)
opInfo 0xE5 = Just ("SBC", ZEROPAGE)
opInfo 0xF5 = Just ("SBC", ZEROPAGE_X)
opInfo 0xED = Just ("SBC", ABSOLUTE)
opInfo 0xFD = Just ("SBC", ABSOLUTE_X)
opInfo 0xF9 = Just ("SBC", ABSOLUTE_Y)
opInfo 0xE1 = Just ("SBC", INDIRECT_X)
opInfo 0xF1 = Just ("SBC", INDIRECT_Y)
opInfo 0x38 = Just ("SEC", IMPLICIT)
opInfo 0xF8 = Just ("SED", IMPLICIT)
opInfo 0x78 = Just ("SEI", IMPLICIT)
opInfo 0x85 = Just ("STA", ZEROPAGE)
opInfo 0x95 = Just ("STA", ZEROPAGE_X)
opInfo 0x8D = Just ("STA", ABSOLUTE)
opInfo 0x9D = Just ("STA", ABSOLUTE_X)
opInfo 0x99 = Just ("STA", ABSOLUTE_Y)
opInfo 0x81 = Just ("STA", INDIRECT_X)
opInfo 0x91 = Just ("STA", INDIRECT_Y)
opInfo 0x86 = Just ("STX", ZEROPAGE)
opInfo 0x96 = Just ("STX", ZEROPAGE_Y)
opInfo 0x8E = Just ("STX", ABSOLUTE)
opInfo 0x84 = Just ("STY", ZEROPAGE)
opInfo 0x94 = Just ("STY", ZEROPAGE_X)
opInfo 0x8C = Just ("STY", ABSOLUTE)
opInfo 0xAA = Just ("TAX", IMPLICIT)
opInfo 0xA8 = Just ("TAY", IMPLICIT)
opInfo 0xBA = Just ("TSX", IMPLICIT)
opInfo 0x8A = Just ("TXA", IMPLICIT)
opInfo 0x9A = Just ("TXS", IMPLICIT)
opInfo 0x98 = Just ("TYA", IMPLICIT)
opInfo opcode = Nothing


disassembleArg :: Interface -> ADDR_MODE -> Word16 -> IO (String, Word16)
disassembleArg _ IMPLICIT _ = return ("", 0)
disassembleArg _ ACCUMULATOR _ = return ("A", 0)
disassembleArg interface IMMEDIATE addr = do
    argval <- iPeekByte interface addr
    return ("#$" ++ toHex1 argval, 1)
disassembleArg interface ZEROPAGE addr = do
    argval <- iPeekByte interface addr
    return ("$" ++ toHex1 argval, 1)
disassembleArg interface ZEROPAGE_X addr = do
    argval <- iPeekByte interface addr
    return ("$" ++ toHex1 argval ++ ",X", 1)
disassembleArg interface ZEROPAGE_Y addr = do
    argval <- iPeekByte interface addr
    return ("$" ++ toHex1 argval ++ ",Y", 1)
disassembleArg interface RELATIVE addr = do
    argval <- iPeekByte interface addr
    let offset = (fromIntegral argval :: Int8)
    return ("*" ++ toHex1 argval ++ " [" ++ show offset ++ "]", 1)
disassembleArg interface ABSOLUTE addr = do
    a1 <- iPeekByte interface addr
    a2 <- iPeekByte interface (addr + 1)
    return ("$" ++ toHex1 a2 ++ toHex1 a1 , 2)
disassembleArg interface ABSOLUTE_X addr = do
    a1 <- iPeekByte interface addr
    a2 <- iPeekByte interface (addr + 1)
    return ("$" ++ toHex1 a2 ++ toHex1 a1 ++ ",X", 2)
disassembleArg interface ABSOLUTE_Y addr = do
    a1 <- iPeekByte interface addr
    a2 <- iPeekByte interface (addr + 1)
    return ("$" ++ toHex1 a2 ++ toHex1 a1 ++ ",Y", 2)
disassembleArg interface INDIRECT addr = do
    a1 <- iPeekByte interface addr
    a2 <- iPeekByte interface (addr + 1)
    return ("($" ++ toHex1 a2 ++ toHex1 a1 ++ ")", 2)
disassembleArg interface INDIRECT_X addr = do
    a <- iPeekByte interface addr
    return ("($" ++ toHex1 a ++ ", X)", 1)
disassembleArg interface INDIRECT_Y addr = do
    a <- iPeekByte interface addr
    return ("($" ++ toHex1 a ++ "), Y", 1)

disassemble :: Interface -> Word16 -> IO (String, Word16)
disassemble interface addr = do
    opcode <- iPeekByte interface addr
    let info = opInfo opcode
    case info of
        Just (opname, addr_mode) -> do
            (args, offset) <- disassembleArg interface addr_mode (addr + 1)
            let str = printf "%05X : %02X    %10s   %15s              [ %12s ]" addr opcode opname args (show addr_mode) :: String
            return (str, offset + 1)
        Nothing -> do
            let str = printf "%05X : %02X    %10s   %15s              [ %12s ]" addr opcode "???" "???" "???" :: String
            return (str, 1)

overflows :: Word16 -> Word16 -> Bool
overflows a b = s < a || s < b where
    s = a + b

disassembleL :: Interface -> Word16 -> Word16 -> IO [(Word16, String)]
disassembleL interface start end 
    | start >= end = return []
    | otherwise = do
        (str, offset) <- disassemble interface start
        let start' = if overflows start offset then end else start + offset
        rest <- disassembleL interface start' end 
        return $ (start, str) : rest

disassembleM :: Interface -> Word16 -> Word16 -> IO (Map.Map Word16 String)
disassembleM interface start end = Map.fromList <$> disassembleL interface start end

binaryInterface :: [Word8] -> Interface
binaryInterface bin = Interface {iReadByte = r, iWriteByte = w, iPeekByte = p} where
    r a = return $ bin !! fromIntegral a
    w _ _ = return ()
    p = r

disassembleL' :: [Word8] -> IO [(Word16, String)]
disassembleL' bin = disassembleL (binaryInterface bin) 0 end where
    end = fromIntegral $ length bin

disassembleM' :: [Word8] -> IO (Map.Map Word16 String)
disassembleM' bin = disassembleM (binaryInterface bin) 0 end where
    end = fromIntegral $ length bin
