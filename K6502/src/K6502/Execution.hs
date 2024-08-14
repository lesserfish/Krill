module K6502.Execution where

import K6502.Types
import K6502.Internal
import K6502.Instructions

import Control.Monad.State
import Data.Word

fetch :: StateT (K6502, Interface) IO Word8
fetch = do
    pc <- offsetIP 1
    opcode <- readByte pc
    setSuperInstruction False
    return opcode

execute :: Word8 -> StateT (K6502, Interface) IO ()
execute 0x69 = do
    offsetCycles 2
    opADC IMMEDIATE
execute 0x65 = do
    offsetCycles 3
    opADC ZEROPAGE
execute 0x75 = do
    offsetCycles 4
    opADC ZEROPAGE_X
execute 0x6D = do
    offsetCycles 4
    opADC ABSOLUTE
execute 0x7D = do
    offsetCycles 4
    setSuperInstruction True
    opADC ABSOLUTE_X
execute 0x79 = do
    offsetCycles 4
    setSuperInstruction True
    opADC ABSOLUTE_Y
execute 0x61 = do
    offsetCycles 6
    opADC INDIRECT_X
execute 0x71 = do
    offsetCycles 5
    setSuperInstruction True
    opADC INDIRECT_Y
execute 0x29 = do
    offsetCycles 2
    opAND IMMEDIATE
execute 0x25 = do
    offsetCycles 3
    opAND ZEROPAGE
execute 0x35 = do
    offsetCycles 4
    opAND ZEROPAGE_X
execute 0x2D = do
    offsetCycles 4
    opAND ABSOLUTE
execute 0x3D = do
    offsetCycles 4
    setSuperInstruction True
    opAND ABSOLUTE_X
execute 0x39 = do
    offsetCycles 4
    setSuperInstruction True
    opAND ABSOLUTE_Y
execute 0x21 = do
    offsetCycles 6
    opAND INDIRECT_X
execute 0x31 = do
    offsetCycles 5
    setSuperInstruction True
    opAND INDIRECT_Y
execute 0x0A = do
    offsetCycles 2
    opASL ACCUMULATOR
execute 0x06 = do
    offsetCycles 5
    opASL ZEROPAGE
execute 0x16 = do
    offsetCycles 6
    opASL ZEROPAGE_X
execute 0x0E = do
    offsetCycles 6
    opASL ABSOLUTE
execute 0x1E = do
    offsetCycles 7
    opASL ABSOLUTE_X
execute 0x90 = do
    offsetCycles 2
    opBCC RELATIVE
execute 0xB0 = do
    offsetCycles 2
    opBCS RELATIVE
execute 0xF0 = do
    offsetCycles 2
    opBEQ RELATIVE
execute 0x24 = do
    offsetCycles 3
    opBIT ZEROPAGE
execute 0x2C = do
    offsetCycles 4
    opBIT ABSOLUTE
execute 0x30 = do
    offsetCycles 2
    opBMI RELATIVE
execute 0xD0 = do
    offsetCycles 2
    opBNE RELATIVE
execute 0x10 = do
    offsetCycles 2
    opBPL RELATIVE
execute 0x00 = do
    offsetCycles 7
    opBRK IMPLICIT
execute 0x50 = do
    offsetCycles 2
    opBVC RELATIVE
execute 0x70 = do
    offsetCycles 2
    opBVS RELATIVE
execute 0x18 = do
    offsetCycles 2
    opCLC IMPLICIT
execute 0xD8 = do
    offsetCycles 2
    opCLD IMPLICIT
execute 0x58 = do
    offsetCycles 2
    opCLI IMPLICIT
execute 0xB8 = do
    offsetCycles 2
    opCLV IMPLICIT
execute 0xC9 = do
    offsetCycles 2
    opCMP IMMEDIATE
execute 0xC5 = do
    offsetCycles 3
    opCMP ZEROPAGE
execute 0xD5 = do
    offsetCycles 4
    opCMP ZEROPAGE_X
execute 0xCD = do
    offsetCycles 4
    opCMP ABSOLUTE
execute 0xDD = do
    offsetCycles 4
    setSuperInstruction True
    opCMP ABSOLUTE_X
execute 0xD9 = do
    offsetCycles 4
    setSuperInstruction True
    opCMP ABSOLUTE_Y
execute 0xC1 = do
    offsetCycles 6
    opCMP INDIRECT_X
execute 0xD1 = do
    offsetCycles 5
    setSuperInstruction True
    opCMP INDIRECT_Y
execute 0xE0 = do
    offsetCycles 2
    opCPX IMMEDIATE
execute 0xE4 = do
    offsetCycles 3
    opCPX ZEROPAGE
execute 0xEC = do
    offsetCycles 4
    opCPX ABSOLUTE
execute 0xC0 = do
    offsetCycles 2
    opCPY IMMEDIATE
execute 0xC4 = do
    offsetCycles 3
    opCPY ZEROPAGE
execute 0xCC = do
    offsetCycles 4
    opCPY ABSOLUTE
execute 0xC6 = do
    offsetCycles 5
    opDEC ZEROPAGE
execute 0xD6 = do
    offsetCycles 6
    opDEC ZEROPAGE_X
execute 0xCE = do
    offsetCycles 6
    opDEC ABSOLUTE
execute 0xDE = do
    offsetCycles 7
    opDEC ABSOLUTE_X
execute 0xCA = do
    offsetCycles 2
    opDEX IMPLICIT
execute 0x88 = do
    offsetCycles 2
    opDEY IMPLICIT
execute 0x49 = do
    offsetCycles 2
    opEOR IMMEDIATE
execute 0x45 = do
    offsetCycles 3
    opEOR ZEROPAGE
execute 0x55 = do
    offsetCycles 4
    opEOR ZEROPAGE_X
execute 0x4D = do
    offsetCycles 4
    opEOR ABSOLUTE
execute 0x5D = do
    offsetCycles 4
    setSuperInstruction True
    opEOR ABSOLUTE_X
execute 0x59 = do
    offsetCycles 4
    setSuperInstruction True
    opEOR ABSOLUTE_Y
execute 0x41 = do
    offsetCycles 6
    opEOR INDIRECT_X
execute 0x51 = do
    offsetCycles 5
    setSuperInstruction True
    opEOR INDIRECT_Y
execute 0xE6 = do
    offsetCycles 5
    opINC ZEROPAGE
execute 0xF6 = do
    offsetCycles 6
    opINC ZEROPAGE_X
execute 0xEE = do
    offsetCycles 6
    opINC ABSOLUTE
execute 0xFE = do
    offsetCycles 7
    opINC ABSOLUTE_X
execute 0xE8 = do
    offsetCycles 2
    opINX IMPLICIT
execute 0xC8 = do
    offsetCycles 2
    opINY IMPLICIT
execute 0x4C = do
    offsetCycles 3
    opJMP ABSOLUTE
execute 0x6C = do
    offsetCycles 5
    opJMP INDIRECT
execute 0x20 = do
    offsetCycles 6
    opJSR ABSOLUTE
execute 0xA9 = do
    offsetCycles 2
    opLDA IMMEDIATE
execute 0xA5 = do
    offsetCycles 3
    opLDA ZEROPAGE
execute 0xB5 = do
    offsetCycles 4
    opLDA ZEROPAGE_X
execute 0xAD = do
    offsetCycles 4
    opLDA ABSOLUTE
execute 0xBD = do
    offsetCycles 4
    setSuperInstruction True
    opLDA ABSOLUTE_X
execute 0xB9 = do
    offsetCycles 4
    setSuperInstruction True
    opLDA ABSOLUTE_Y
execute 0xA1 = do
    offsetCycles 6
    opLDA INDIRECT_X
execute 0xB1 = do
    offsetCycles 5
    setSuperInstruction True
    opLDA INDIRECT_Y
execute 0xA2 = do
    offsetCycles 2
    opLDX IMMEDIATE
execute 0xA6 = do
    offsetCycles 3
    opLDX ZEROPAGE
execute 0xB6 = do
    offsetCycles 4
    opLDX ZEROPAGE_Y
execute 0xAE = do
    offsetCycles 4
    opLDX ABSOLUTE
execute 0xBE = do
    offsetCycles 4
    setSuperInstruction True
    opLDX ABSOLUTE_Y
execute 0xA0 = do
    offsetCycles 2
    opLDY IMMEDIATE
execute 0xA4 = do
    offsetCycles 3
    opLDY ZEROPAGE
execute 0xB4 = do
    offsetCycles 4
    opLDY ZEROPAGE_X
execute 0xAC = do
    offsetCycles 4
    opLDY ABSOLUTE
execute 0xBC = do
    offsetCycles 4
    setSuperInstruction True
    opLDY ABSOLUTE_X
execute 0x4A = do
    offsetCycles 2
    opLSR ACCUMULATOR
execute 0x46 = do
    offsetCycles 5
    opLSR ZEROPAGE
execute 0x56 = do
    offsetCycles 6
    opLSR ZEROPAGE_X
execute 0x4E = do
    offsetCycles 6
    opLSR ABSOLUTE
execute 0x5E = do
    offsetCycles 7
    opLSR ABSOLUTE_X
execute 0xEA = do
    offsetCycles 2
    opNOP IMPLICIT -- ?
execute 0x09 = do
    offsetCycles 2
    opORA IMMEDIATE
execute 0x05 = do
    offsetCycles 3
    opORA ZEROPAGE
execute 0x15 = do
    offsetCycles 4
    opORA ZEROPAGE_X
execute 0x0D = do
    offsetCycles 4
    opORA ABSOLUTE
execute 0x1D = do
    offsetCycles 4
    setSuperInstruction True
    opORA ABSOLUTE_X
execute 0x19 = do
    offsetCycles 4
    setSuperInstruction True
    opORA ABSOLUTE_Y
execute 0x01 = do
    offsetCycles 6
    opORA INDIRECT_X
execute 0x11 = do
    offsetCycles 5
    setSuperInstruction True
    opORA INDIRECT_Y
execute 0x48 = do
    offsetCycles 3
    opPHA IMPLICIT
execute 0x08 = do
    offsetCycles 3
    opPHP IMPLICIT
execute 0x68 = do
    offsetCycles 4
    opPLA IMPLICIT
execute 0x28 = do
    offsetCycles 4
    opPLP IMPLICIT
execute 0x2A = do
    offsetCycles 2
    opROL ACCUMULATOR
execute 0x26 = do
    offsetCycles 5
    opROL ZEROPAGE
execute 0x36 = do
    offsetCycles 6
    opROL ZEROPAGE_X
execute 0x2E = do
    offsetCycles 6
    opROL ABSOLUTE
execute 0x3E = do
    offsetCycles 7
    opROL ABSOLUTE_X
execute 0x6A = do
    offsetCycles 2
    opROR ACCUMULATOR
execute 0x66 = do
    offsetCycles 5
    opROR ZEROPAGE
execute 0x76 = do
    offsetCycles 6
    opROR ZEROPAGE_X
execute 0x6E = do
    offsetCycles 6
    opROR ABSOLUTE
execute 0x7E = do
    offsetCycles 7
    opROR ABSOLUTE_X
execute 0x40 = do
    offsetCycles 6
    opRTI IMPLICIT
execute 0x60 = do
    offsetCycles 6
    opRTS IMPLICIT
execute 0xE9 = do
    offsetCycles 2
    opSBC IMMEDIATE
execute 0xE5 = do
    offsetCycles 3
    opSBC ZEROPAGE
execute 0xF5 = do
    offsetCycles 4
    opSBC ZEROPAGE_X
execute 0xED = do
    offsetCycles 4
    opSBC ABSOLUTE
execute 0xFD = do
    offsetCycles 4
    setSuperInstruction True
    opSBC ABSOLUTE_X
execute 0xF9 = do
    offsetCycles 4
    setSuperInstruction True
    opSBC ABSOLUTE_Y
execute 0xE1 = do
    offsetCycles 6
    opSBC INDIRECT_X
execute 0xF1 = do
    offsetCycles 5
    setSuperInstruction True
    opSBC INDIRECT_Y
execute 0x38 = do
    offsetCycles 2
    opSEC IMPLICIT
execute 0xF8 = do
    offsetCycles 2
    opSED IMPLICIT
execute 0x78 = do
    offsetCycles 2
    opSEI IMPLICIT
execute 0x85 = do
    offsetCycles 3
    opSTA ZEROPAGE
execute 0x95 = do
    offsetCycles 4
    opSTA ZEROPAGE_X
execute 0x8D = do
    offsetCycles 4
    opSTA ABSOLUTE
execute 0x9D = do
    offsetCycles 5
    opSTA ABSOLUTE_X
execute 0x99 = do
    offsetCycles 5
    opSTA ABSOLUTE_Y
execute 0x81 = do
    offsetCycles 6
    opSTA INDIRECT_X
execute 0x91 = do
    offsetCycles 6
    opSTA INDIRECT_Y
execute 0x86 = do
    offsetCycles 3
    opSTX ZEROPAGE
execute 0x96 = do
    offsetCycles 4
    opSTX ZEROPAGE_Y
execute 0x8E = do
    offsetCycles 4
    opSTX ABSOLUTE
execute 0x84 = do
    offsetCycles 3
    opSTY ZEROPAGE
execute 0x94 = do
    offsetCycles 4
    opSTY ZEROPAGE_X
execute 0x8C = do
    offsetCycles 4
    opSTY ABSOLUTE
execute 0xAA = do
    offsetCycles 2
    opTAX IMPLICIT
execute 0xA8 = do
    offsetCycles 2
    opTAY IMPLICIT
execute 0xBA = do
    offsetCycles 2
    opTSX IMPLICIT
execute 0x8A = do
    offsetCycles 2
    opTXA IMPLICIT
execute 0x9A = do
    offsetCycles 2
    opTXS IMPLICIT
execute 0x98 = do
    offsetCycles 2
    opTYA IMPLICIT
execute _ = return ()

