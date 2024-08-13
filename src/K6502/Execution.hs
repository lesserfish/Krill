module K6502.Execution where

{--

fetch :: StateT (MOS6502 a, a) IO Word8
fetch = do
    pc <- getPC
    opcode <- readByte pc
    setPC (pc + 1)
    setSuperInstruction False
    return opcode



execute :: Word8 -> StateT (MOS6502 a, a) IO ()
execute 0x69 = do
    updateCycles 2
    opADC IMMEDIATE
execute 0x65 = do
    updateCycles 3
    opADC ZEROPAGE
execute 0x75 = do
    updateCycles 4
    opADC ZEROPAGE_X
execute 0x6D = do
    updateCycles 4
    opADC ABSOLUTE
execute 0x7D = do
    updateCycles 4
    setSuperInstruction True
    opADC ABSOLUTE_X
execute 0x79 = do
    updateCycles 4
    setSuperInstruction True
    opADC ABSOLUTE_Y
execute 0x61 = do
    updateCycles 6
    opADC INDIRECT_X
execute 0x71 = do
    updateCycles 5
    setSuperInstruction True
    opADC INDIRECT_Y
execute 0x29 = do
    updateCycles 2
    opAND IMMEDIATE
execute 0x25 = do
    updateCycles 3
    opAND ZEROPAGE
execute 0x35 = do
    updateCycles 4
    opAND ZEROPAGE_X
execute 0x2D = do
    updateCycles 4
    opAND ABSOLUTE
execute 0x3D = do
    updateCycles 4
    setSuperInstruction True
    opAND ABSOLUTE_X
execute 0x39 = do
    updateCycles 4
    setSuperInstruction True
    opAND ABSOLUTE_Y
execute 0x21 = do
    updateCycles 6
    opAND INDIRECT_X
execute 0x31 = do
    updateCycles 5
    setSuperInstruction True
    opAND INDIRECT_Y
execute 0x0A = do
    updateCycles 2
    opASL ACCUMULATOR
execute 0x06 = do
    updateCycles 5
    opASL ZEROPAGE
execute 0x16 = do
    updateCycles 6
    opASL ZEROPAGE_X
execute 0x0E = do
    updateCycles 6
    opASL ABSOLUTE
execute 0x1E = do
    updateCycles 7
    opASL ABSOLUTE_X
execute 0x90 = do
    updateCycles 2
    opBCC RELATIVE
execute 0xB0 = do
    updateCycles 2
    opBCS RELATIVE
execute 0xF0 = do
    updateCycles 2
    opBEQ RELATIVE
execute 0x24 = do
    updateCycles 3
    opBIT ZEROPAGE
execute 0x2C = do
    updateCycles 4
    opBIT ABSOLUTE
execute 0x30 = do
    updateCycles 2
    opBMI RELATIVE
execute 0xD0 = do
    updateCycles 2
    opBNE RELATIVE
execute 0x10 = do
    updateCycles 2
    opBPL RELATIVE
execute 0x00 = do
    updateCycles 7
    opBRK IMPLICIT
execute 0x50 = do
    updateCycles 2
    opBVC RELATIVE
execute 0x70 = do
    updateCycles 2
    opBVS RELATIVE
execute 0x18 = do
    updateCycles 2
    opCLC IMPLICIT
execute 0xD8 = do
    updateCycles 2
    opCLD IMPLICIT
execute 0x58 = do
    updateCycles 2
    opCLI IMPLICIT
execute 0xB8 = do
    updateCycles 2
    opCLV IMPLICIT
execute 0xC9 = do
    updateCycles 2
    opCMP IMMEDIATE
execute 0xC5 = do
    updateCycles 3
    opCMP ZEROPAGE
execute 0xD5 = do
    updateCycles 4
    opCMP ZEROPAGE_X
execute 0xCD = do
    updateCycles 4
    opCMP ABSOLUTE
execute 0xDD = do
    updateCycles 4
    setSuperInstruction True
    opCMP ABSOLUTE_X
execute 0xD9 = do
    updateCycles 4
    setSuperInstruction True
    opCMP ABSOLUTE_Y
execute 0xC1 = do
    updateCycles 6
    opCMP INDIRECT_X
execute 0xD1 = do
    updateCycles 5
    setSuperInstruction True
    opCMP INDIRECT_Y
execute 0xE0 = do
    updateCycles 2
    opCPX IMMEDIATE
execute 0xE4 = do
    updateCycles 3
    opCPX ZEROPAGE
execute 0xEC = do
    updateCycles 4
    opCPX ABSOLUTE
execute 0xC0 = do
    updateCycles 2
    opCPY IMMEDIATE
execute 0xC4 = do
    updateCycles 3
    opCPY ZEROPAGE
execute 0xCC = do
    updateCycles 4
    opCPY ABSOLUTE
execute 0xC6 = do
    updateCycles 5
    opDEC ZEROPAGE
execute 0xD6 = do
    updateCycles 6
    opDEC ZEROPAGE_X
execute 0xCE = do
    updateCycles 6
    opDEC ABSOLUTE
execute 0xDE = do
    updateCycles 7
    opDEC ABSOLUTE_X
execute 0xCA = do
    updateCycles 2
    opDEX IMPLICIT
execute 0x88 = do
    updateCycles 2
    opDEY IMPLICIT
execute 0x49 = do
    updateCycles 2
    opEOR IMMEDIATE
execute 0x45 = do
    updateCycles 3
    opEOR ZEROPAGE
execute 0x55 = do
    updateCycles 4
    opEOR ZEROPAGE_X
execute 0x4D = do
    updateCycles 4
    opEOR ABSOLUTE
execute 0x5D = do
    updateCycles 4
    setSuperInstruction True
    opEOR ABSOLUTE_X
execute 0x59 = do
    updateCycles 4
    setSuperInstruction True
    opEOR ABSOLUTE_Y
execute 0x41 = do
    updateCycles 6
    opEOR INDIRECT_X
execute 0x51 = do
    updateCycles 5
    setSuperInstruction True
    opEOR INDIRECT_Y
execute 0xE6 = do
    updateCycles 5
    opINC ZEROPAGE
execute 0xF6 = do
    updateCycles 6
    opINC ZEROPAGE_X
execute 0xEE = do
    updateCycles 6
    opINC ABSOLUTE
execute 0xFE = do
    updateCycles 7
    opINC ABSOLUTE_X
execute 0xE8 = do
    updateCycles 2
    opINX IMPLICIT
execute 0xC8 = do
    updateCycles 2
    opINY IMPLICIT
execute 0x4C = do
    updateCycles 3
    opJMP ABSOLUTE
execute 0x6C = do
    updateCycles 5
    opJMP INDIRECT
execute 0x20 = do
    updateCycles 6
    opJSR ABSOLUTE
execute 0xA9 = do
    updateCycles 2
    opLDA IMMEDIATE
execute 0xA5 = do
    updateCycles 3
    opLDA ZEROPAGE
execute 0xB5 = do
    updateCycles 4
    opLDA ZEROPAGE_X
execute 0xAD = do
    updateCycles 4
    opLDA ABSOLUTE
execute 0xBD = do
    updateCycles 4
    setSuperInstruction True
    opLDA ABSOLUTE_X
execute 0xB9 = do
    updateCycles 4
    setSuperInstruction True
    opLDA ABSOLUTE_Y
execute 0xA1 = do
    updateCycles 6
    opLDA INDIRECT_X
execute 0xB1 = do
    updateCycles 5
    setSuperInstruction True
    opLDA INDIRECT_Y
execute 0xA2 = do
    updateCycles 2
    opLDX IMMEDIATE
execute 0xA6 = do
    updateCycles 3
    opLDX ZEROPAGE
execute 0xB6 = do
    updateCycles 4
    opLDX ZEROPAGE_Y
execute 0xAE = do
    updateCycles 4
    opLDX ABSOLUTE
execute 0xBE = do
    updateCycles 4
    setSuperInstruction True
    opLDX ABSOLUTE_Y
execute 0xA0 = do
    updateCycles 2
    opLDY IMMEDIATE
execute 0xA4 = do
    updateCycles 3
    opLDY ZEROPAGE
execute 0xB4 = do
    updateCycles 4
    opLDY ZEROPAGE_X
execute 0xAC = do
    updateCycles 4
    opLDY ABSOLUTE
execute 0xBC = do
    updateCycles 4
    setSuperInstruction True
    opLDY ABSOLUTE_X
execute 0x4A = do
    updateCycles 2
    opLSR ACCUMULATOR
execute 0x46 = do
    updateCycles 5
    opLSR ZEROPAGE
execute 0x56 = do
    updateCycles 6
    opLSR ZEROPAGE_X
execute 0x4E = do
    updateCycles 6
    opLSR ABSOLUTE
execute 0x5E = do
    updateCycles 7
    opLSR ABSOLUTE_X
execute 0xEA = do
    updateCycles 2
    opNOP IMPLICIT -- ?
execute 0x09 = do
    updateCycles 2
    opORA IMMEDIATE
execute 0x05 = do
    updateCycles 3
    opORA ZEROPAGE
execute 0x15 = do
    updateCycles 4
    opORA ZEROPAGE_X
execute 0x0D = do
    updateCycles 4
    opORA ABSOLUTE
execute 0x1D = do
    updateCycles 4
    setSuperInstruction True
    opORA ABSOLUTE_X
execute 0x19 = do
    updateCycles 4
    setSuperInstruction True
    opORA ABSOLUTE_Y
execute 0x01 = do
    updateCycles 6
    opORA INDIRECT_X
execute 0x11 = do
    updateCycles 5
    setSuperInstruction True
    opORA INDIRECT_Y
execute 0x48 = do
    updateCycles 3
    opPHA IMPLICIT
execute 0x08 = do
    updateCycles 3
    opPHP IMPLICIT
execute 0x68 = do
    updateCycles 4
    opPLA IMPLICIT
execute 0x28 = do
    updateCycles 4
    opPLP IMPLICIT
execute 0x2A = do
    updateCycles 2
    opROL ACCUMULATOR
execute 0x26 = do
    updateCycles 5
    opROL ZEROPAGE
execute 0x36 = do
    updateCycles 6
    opROL ZEROPAGE_X
execute 0x2E = do
    updateCycles 6
    opROL ABSOLUTE
execute 0x3E = do
    updateCycles 7
    opROL ABSOLUTE_X
execute 0x6A = do
    updateCycles 2
    opROR ACCUMULATOR
execute 0x66 = do
    updateCycles 5
    opROR ZEROPAGE
execute 0x76 = do
    updateCycles 6
    opROR ZEROPAGE_X
execute 0x6E = do
    updateCycles 6
    opROR ABSOLUTE
execute 0x7E = do
    updateCycles 7
    opROR ABSOLUTE_X
execute 0x40 = do
    updateCycles 6
    opRTI IMPLICIT
execute 0x60 = do
    updateCycles 6
    opRTS IMPLICIT
execute 0xE9 = do
    updateCycles 2
    opSBC IMMEDIATE
execute 0xE5 = do
    updateCycles 3
    opSBC ZEROPAGE
execute 0xF5 = do
    updateCycles 4
    opSBC ZEROPAGE_X
execute 0xED = do
    updateCycles 4
    opSBC ABSOLUTE
execute 0xFD = do
    updateCycles 4
    setSuperInstruction True
    opSBC ABSOLUTE_X
execute 0xF9 = do
    updateCycles 4
    setSuperInstruction True
    opSBC ABSOLUTE_Y
execute 0xE1 = do
    updateCycles 6
    opSBC INDIRECT_X
execute 0xF1 = do
    updateCycles 5
    setSuperInstruction True
    opSBC INDIRECT_Y
execute 0x38 = do
    updateCycles 2
    opSEC IMPLICIT
execute 0xF8 = do
    updateCycles 2
    opSED IMPLICIT
execute 0x78 = do
    updateCycles 2
    opSEI IMPLICIT
execute 0x85 = do
    updateCycles 3
    opSTA ZEROPAGE
execute 0x95 = do
    updateCycles 4
    opSTA ZEROPAGE_X
execute 0x8D = do
    updateCycles 4
    opSTA ABSOLUTE
execute 0x9D = do
    updateCycles 5
    opSTA ABSOLUTE_X
execute 0x99 = do
    updateCycles 5
    opSTA ABSOLUTE_Y
execute 0x81 = do
    updateCycles 6
    opSTA INDIRECT_X
execute 0x91 = do
    updateCycles 6
    opSTA INDIRECT_Y
execute 0x86 = do
    updateCycles 3
    opSTX ZEROPAGE
execute 0x96 = do
    updateCycles 4
    opSTX ZEROPAGE_Y
execute 0x8E = do
    updateCycles 4
    opSTX ABSOLUTE
execute 0x84 = do
    updateCycles 3
    opSTY ZEROPAGE
execute 0x94 = do
    updateCycles 4
    opSTY ZEROPAGE_X
execute 0x8C = do
    updateCycles 4
    opSTY ABSOLUTE
execute 0xAA = do
    updateCycles 2
    opTAX IMPLICIT
execute 0xA8 = do
    updateCycles 2
    opTAY IMPLICIT
execute 0xBA = do
    updateCycles 2
    opTSX IMPLICIT
execute 0x8A = do
    updateCycles 2
    opTXA IMPLICIT
execute 0x9A = do
    updateCycles 2
    opTXS IMPLICIT
execute 0x98 = do
    updateCycles 2
    opTYA IMPLICIT
execute opcode = return ()
--error (show opcode ++ ": Unknown opcode") -- TODO: Add error log to Context perhaps?

--}
