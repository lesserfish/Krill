module K6502.Instructions where

{--
reset ::StateT (MOS6502 a, a) IO () -- Non-Maskable Interrupt
reset = do
    irq_lb <- readByte 0xFFFC -- Get the NMI interrupt vector
    irq_hb <- readByte 0xFFFD --
    let jmp_addr = joinBytes irq_hb irq_lb
    setPC  jmp_addr -- Set the PC Register
    setSP  0xFD
    setACC 0x00
    setIDX 0x00
    setIDY 0x00
    setPS (setBit 0x00 5 :: Word8) -- Set the Status Flag. (UNUSED flag set to 1)
    resetClock
    resetCycles
    resetCounter

-- TODO: Reset CPU Context

-- INSTRUCTIONS:
--
-- opXXX IMPLICIT = error "Operation XXX does not support IMPLICIT addressing mode"
-- opXXX ACCUMULATOR = error "Operation XXX does not support ACCUMULATOR addressing mode"
-- opXXX IMMEDIATE = error "Operation XXX does not support IMMEDIATE addressing mode"
-- opXXX ZEROPAGE = error "Operation XXX does not support ZEROPAGE addressing mode"
-- opXXX ZEROPAGE_X = error "Operation XXX does not support ZEROPAGE_X addressing mode"
-- opXXX ZEROPAGE_Y = error "Operation XXX does not support ZEROPAGE_Y addressing mode"
-- opXXX RELATIVE = error "Operation XXX does not support RELATIVE addressing mode"
-- opXXX ABSOLUTE = error "Operation XXX does not support ABSOLUTE addressing mode"
-- opXXX ABSOLUTE_X = error "Operation XXX does not support ABSOLUTE_X addressing mode"
-- opXXX ABSOLUTE_Y = error "Operation XXX does not support ABSOLUTE_Y addressing mode"
-- opXXX INDIRECT = error "Operation XXX does not support INDIRECT addressing mode"
-- opXXX INDIRECT_X = error "Operation XXX does not support INDIRECT_X addressing mode"
-- opXXX INDIRECT_Y = error "Operation XXX does not support INDIRECT_Y addressing mode"
--
--
-- Note: Instructions do not have to update the PC unless they use additional operands through the means of addressing modes.
-- The PC should be updated before the call to the instruction.

opADC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opADC IMPLICIT = error "Operation ADC does not support IMPLICIT addressing mode"
opADC ACCUMULATOR = error "Operation ADC does not support ACCUMULATOR addressing mode"
opADC ZEROPAGE_Y = error "Operation ADC does not support ZEROPAGE_Y addressing mode"
opADC RELATIVE = error "Operation ADC does not support RELATIVE addressing mode"
opADC INDIRECT = error "Operation ADC does not support INDIRECT addressing mode"
opADC addr_mode = do
    acc <- getACC 
    carry_flag <- getFlag $ CARRY -- Get the Accumulator registers prior to changes
    let carry = if carry_flag then 1 else 0 :: Word8
    decimal_flag <- getFlag $ DECIMAL_MODE
    decMode <- getDecMode
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let iacc = fromIntegral acc :: Int
    let ibyte = fromIntegral byte :: Int
    let icarry = fromIntegral carry :: Int
    let result = iacc + ibyte + icarry
    setFlag ZERO ((result .&. 0xFF) == 0) -- Sets the Zero flag if the result is equal to 0
    if decimal_flag && decMode
        then do
            let ln = (fromIntegral acc .&. 0xF) + (fromIntegral byte .&. 0xF) + fromIntegral carry :: Word16
            let ln' = if (ln >= 0xA) then ((ln + 0x6) .&. 0xF) + 0x10 else ln
            let r = (fromIntegral acc .&. 0xF0) + (fromIntegral byte .&. 0xF0) + ln' :: Word16
            setFlag NEGATIVE (b7 r)
            -- setFlag OVERFLOW (not (b7' (iacc `xor` ibyte)) && (b7' (iacc `xor` result)))
            setFlag OVERFLOW (((((r `xor` fromIntegral acc) .&. (r `xor` fromIntegral byte)) .&. 0x80) .>>. 1) /= 0)
            let r' = if r >= 0xA0 then r + 0x60 else r
            setFlag CARRY (r' .>>. 8 /= 0)
            let acc' = fromIntegral r' :: Word8
            setACC acc'
        else do
            setFlag NEGATIVE (bi7 result)
            setFlag OVERFLOW (not (bi7 (iacc `xor` ibyte)) && (bi7 (iacc `xor` result)))
            setFlag CARRY (result > 0xFF)
            let acc' = fromIntegral (result .&. 0xFF) :: Word8
            setACC acc'

opAND ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opAND IMPLICIT = error "Operation AND does not support IMPLICIT addressing mode"
opAND ACCUMULATOR = error "Operation AND does not support ACCUMULATOR addressing mode"
opAND ZEROPAGE_Y = error "Operation AND does not support ZEROPAGE_Y addressing mode"
opAND RELATIVE = error "Operation AND does not support RELATIVE addressing mode"
opAND INDIRECT = error "Operation AND does not support INDIRECT addressing mode"
opAND addr_mode = do
    old_acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    mapACC (.&. byte) -- AND the corresponding byte to Accumulator
    acc <- getACC 
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' acc) -- Sets the Negative flag is the result is negative

opASL ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opASL IMPLICIT = error "Operation ASL does not support IMPLICIT addressing mode"
opASL IMMEDIATE = error "Operation ASL does not support IMMEDIATE addressing mode"
opASL ZEROPAGE_Y = error "Operation ASL does not support ZEROPAGE_Y addressing mode"
opASL RELATIVE = error "Operation ASL does not support RELATIVE addressing mode"
opASL ABSOLUTE_Y = error "Operation ASL does not support ABSOLUTE_Y addressing mode"
opASL INDIRECT = error "Operation ASL does not support INDIRECT addressing mode"
opASL INDIRECT_X = error "Operation ASL does not support INDIRECT_X addressing mode"
opASL INDIRECT_Y = error "Operation ASL does not support INDIRECT_Y addressing mode"
opASL ACCUMULATOR = do
    old_acc <- getACC 
    let carry_flag = b7' old_acc -- Carry flag is set to contents of old bit 7
    mapACC ((\x -> x .<<. 1) :: Word8 -> Word8) -- Shifts byte one bit to the left
    acc <- getACC 
    setFlag CARRY carry_flag -- Sets the Carry flag
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' acc) -- Sets the Negative flag is the result is negative
opASL addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let carry_flag = b7' byte -- Carry flag is set to contents of old bit 7
    let new_byte = byte .<<. 1 :: Word8 -- Perform the L Shift
    writeByte addr new_byte -- Write new byte to same address
    setFlag CARRY carry_flag -- Sets the Carry flag
    setFlag ZERO (new_byte == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' new_byte) -- Sets the Negative flag is the result is negative

opBCC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBCC IMPLICIT = error "Operation BCC does not support IMPLICIT addressing mode"
opBCC ACCUMULATOR = error "Operation BCC does not support ACCUMULATOR addressing mode"
opBCC IMMEDIATE = error "Operation BCC does not support IMMEDIATE addressing mode"
opBCC ZEROPAGE = error "Operation BCC does not support ZEROPAGE addressing mode"
opBCC ZEROPAGE_X = error "Operation BCC does not support ZEROPAGE_X addressing mode"
opBCC ZEROPAGE_Y = error "Operation BCC does not support ZEROPAGE_Y addressing mode"
opBCC ABSOLUTE = error "Operation BCC does not support ABSOLUTE addressing mode"
opBCC ABSOLUTE_X = error "Operation BCC does not support ABSOLUTE_X addressing mode"
opBCC ABSOLUTE_Y = error "Operation BCC does not support ABSOLUTE_Y addressing mode"
opBCC INDIRECT = error "Operation BCC does not support INDIRECT addressing mode"
opBCC INDIRECT_X = error "Operation BCC does not support INDIRECT_X addressing mode"
opBCC INDIRECT_Y = error "Operation BCC does not support INDIRECT_Y addressing mode"
opBCC RELATIVE = do
    carry_flag <- getFlag CARRY -- Get carry flag
    when (not carry_flag) (updateCycles 1)
    when (not carry_flag) (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when (not carry_flag) (setPC addr)

opBCS ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBCS IMPLICIT = error "Operation BCS does not support IMPLICIT addressing mode"
opBCS ACCUMULATOR = error "Operation BCS does not support ACCUMULATOR addressing mode"
opBCS IMMEDIATE = error "Operation BCS does not support IMMEDIATE addressing mode"
opBCS ZEROPAGE = error "Operation BCS does not support ZEROPAGE addressing mode"
opBCS ZEROPAGE_X = error "Operation BCS does not support ZEROPAGE_X addressing mode"
opBCS ZEROPAGE_Y = error "Operation BCS does not support ZEROPAGE_Y addressing mode"
opBCS ABSOLUTE = error "Operation BCS does not support ABSOLUTE addressing mode"
opBCS ABSOLUTE_X = error "Operation BCS does not support ABSOLUTE_X addressing mode"
opBCS ABSOLUTE_Y = error "Operation BCS does not support ABSOLUTE_Y addressing mode"
opBCS INDIRECT = error "Operation BCS does not support INDIRECT addressing mode"
opBCS INDIRECT_X = error "Operation BCS does not support INDIRECT_X addressing mode"
opBCS INDIRECT_Y = error "Operation BCS does not support INDIRECT_Y addressing mode"
opBCS RELATIVE = do
    carry_flag <- getFlag CARRY -- Get carry flag
    when carry_flag (updateCycles 1)
    when carry_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when carry_flag (setPC addr)

opBEQ ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBEQ IMPLICIT = error "Operation BEQ does not support IMPLICIT addressing mode"
opBEQ ACCUMULATOR = error "Operation BEQ does not support ACCUMULATOR addressing mode"
opBEQ IMMEDIATE = error "Operation BEQ does not support IMMEDIATE addressing mode"
opBEQ ZEROPAGE = error "Operation BEQ does not support ZEROPAGE addressing mode"
opBEQ ZEROPAGE_X = error "Operation BEQ does not support ZEROPAGE_X addressing mode"
opBEQ ZEROPAGE_Y = error "Operation BEQ does not support ZEROPAGE_Y addressing mode"
opBEQ ABSOLUTE = error "Operation BEQ does not support ABSOLUTE addressing mode"
opBEQ ABSOLUTE_X = error "Operation BEQ does not support ABSOLUTE_X addressing mode"
opBEQ ABSOLUTE_Y = error "Operation BEQ does not support ABSOLUTE_Y addressing mode"
opBEQ INDIRECT = error "Operation BEQ does not support INDIRECT addressing mode"
opBEQ INDIRECT_X = error "Operation BEQ does not support INDIRECT_X addressing mode"
opBEQ INDIRECT_Y = error "Operation BEQ does not support INDIRECT_Y addressing mode"
opBEQ RELATIVE = do
    zero_flag <- getFlag ZERO -- Get Zero flag
    when zero_flag (updateCycles 1)
    when zero_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when zero_flag (setPC addr)

opBIT ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBIT IMPLICIT = error "Operation BIT does not support IMPLICIT addressing mode"
opBIT ACCUMULATOR = error "Operation BIT does not support ACCUMULATOR addressing mode"
opBIT IMMEDIATE = error "Operation BIT does not support IMMEDIATE addressing mode"
opBIT ZEROPAGE_X = error "Operation BIT does not support ZEROPAGE_X addressing mode"
opBIT ZEROPAGE_Y = error "Operation BIT does not support ZEROPAGE_Y addressing mode"
opBIT RELATIVE = error "Operation BIT does not support RELATIVE addressing mode"
opBIT ABSOLUTE_X = error "Operation BIT does not support ABSOLUTE_X addressing mode"
opBIT ABSOLUTE_Y = error "Operation BIT does not support ABSOLUTE_Y addressing mode"
opBIT INDIRECT = error "Operation BIT does not support INDIRECT addressing mode"
opBIT INDIRECT_X = error "Operation BIT does not support INDIRECT_X addressing mode"
opBIT INDIRECT_Y = error "Operation BIT does not support INDIRECT_Y addressing mode"
opBIT addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    acc <- getACC 
    let and_result = (byte .&. acc) -- Perform AND operation
    setFlag ZERO (and_result == 0) -- Sets the ZERO flag if the result of the AND operation is 0
    setFlag NEGATIVE (b7' byte) -- Sets the Negative flag to the seventh bit of the address value
    setFlag OVERFLOW (b6' byte) -- Sets the Overflow flag to the sixth bit of the address value

opBMI ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBMI IMPLICIT = error "Operation BMI does not support IMPLICIT addressing mode"
opBMI ACCUMULATOR = error "Operation BMI does not support ACCUMULATOR addressing mode"
opBMI IMMEDIATE = error "Operation BMI does not support IMMEDIATE addressing mode"
opBMI ZEROPAGE = error "Operation BMI does not support ZEROPAGE addressing mode"
opBMI ZEROPAGE_X = error "Operation BMI does not support ZEROPAGE_X addressing mode"
opBMI ZEROPAGE_Y = error "Operation BMI does not support ZEROPAGE_Y addressing mode"
opBMI ABSOLUTE = error "Operation BMI does not support ABSOLUTE addressing mode"
opBMI ABSOLUTE_X = error "Operation BMI does not support ABSOLUTE_X addressing mode"
opBMI ABSOLUTE_Y = error "Operation BMI does not support ABSOLUTE_Y addressing mode"
opBMI INDIRECT = error "Operation BMI does not support INDIRECT addressing mode"
opBMI INDIRECT_X = error "Operation BMI does not support INDIRECT_X addressing mode"
opBMI INDIRECT_Y = error "Operation BMI does not support INDIRECT_Y addressing mode"
opBMI RELATIVE = do
    negative_flag <- getFlag NEGATIVE -- Get Negative flag
    when negative_flag (updateCycles 1)
    when negative_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when negative_flag (setPC addr)

opBNE ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBNE IMPLICIT = error "Operation BNE does not support IMPLICIT addressing mode"
opBNE ACCUMULATOR = error "Operation BNE does not support ACCUMULATOR addressing mode"
opBNE IMMEDIATE = error "Operation BNE does not support IMMEDIATE addressing mode"
opBNE ZEROPAGE = error "Operation BNE does not support ZEROPAGE addressing mode"
opBNE ZEROPAGE_X = error "Operation BNE does not support ZEROPAGE_X addressing mode"
opBNE ZEROPAGE_Y = error "Operation BNE does not support ZEROPAGE_Y addressing mode"
opBNE ABSOLUTE = error "Operation BNE does not support ABSOLUTE addressing mode"
opBNE ABSOLUTE_X = error "Operation BNE does not support ABSOLUTE_X addressing mode"
opBNE ABSOLUTE_Y = error "Operation BNE does not support ABSOLUTE_Y addressing mode"
opBNE INDIRECT = error "Operation BNE does not support INDIRECT addressing mode"
opBNE INDIRECT_X = error "Operation BNE does not support INDIRECT_X addressing mode"
opBNE INDIRECT_Y = error "Operation BNE does not support INDIRECT_Y addressing mode"
opBNE RELATIVE = do
    zero_flag <- getFlag ZERO -- Get Zero flag
    when (not zero_flag) (updateCycles 1)
    when (not zero_flag) (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when (not zero_flag) (setPC addr)

opBPL ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBPL IMPLICIT = error "Operation BPL does not support IMPLICIT addressing mode"
opBPL ACCUMULATOR = error "Operation BPL does not support ACCUMULATOR addressing mode"
opBPL IMMEDIATE = error "Operation BPL does not support IMMEDIATE addressing mode"
opBPL ZEROPAGE = error "Operation BPL does not support ZEROPAGE addressing mode"
opBPL ZEROPAGE_X = error "Operation BPL does not support ZEROPAGE_X addressing mode"
opBPL ZEROPAGE_Y = error "Operation BPL does not support ZEROPAGE_Y addressing mode"
opBPL ABSOLUTE = error "Operation BPL does not support ABSOLUTE addressing mode"
opBPL ABSOLUTE_X = error "Operation BPL does not support ABSOLUTE_X addressing mode"
opBPL ABSOLUTE_Y = error "Operation BPL does not support ABSOLUTE_Y addressing mode"
opBPL INDIRECT = error "Operation BPL does not support INDIRECT addressing mode"
opBPL INDIRECT_X = error "Operation BPL does not support INDIRECT_X addressing mode"
opBPL INDIRECT_Y = error "Operation BPL does not support INDIRECT_Y addressing mode"
opBPL RELATIVE = do
    negative_flag <- getFlag NEGATIVE -- Get Zero flag
    when (not negative_flag) (updateCycles 1)
    when (not negative_flag) (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when (not negative_flag) (setPC addr)

opBRK ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBRK ACCUMULATOR = error "Operation BRK does not support ACCUMULATOR addressing mode"
opBRK IMMEDIATE = error "Operation BRK does not support IMMEDIATE addressing mode"
opBRK ZEROPAGE = error "Operation BRK does not support ZEROPAGE addressing mode"
opBRK ZEROPAGE_X = error "Operation BRK does not support ZEROPAGE_X addressing mode"
opBRK ZEROPAGE_Y = error "Operation BRK does not support ZEROPAGE_Y addressing mode"
opBRK RELATIVE = error "Operation BRK does not support RELATIVE addressing mode"
opBRK ABSOLUTE = error "Operation BRK does not support ABSOLUTE addressing mode"
opBRK ABSOLUTE_X = error "Operation BRK does not support ABSOLUTE_X addressing mode"
opBRK ABSOLUTE_Y = error "Operation BRK does not support ABSOLUTE_Y addressing mode"
opBRK INDIRECT = error "Operation BRK does not support INDIRECT addressing mode"
opBRK INDIRECT_X = error "Operation BRK does not support INDIRECT_X addressing mode"
opBRK INDIRECT_Y = error "Operation BRK does not support INDIRECT_Y addressing mode"
opBRK IMPLICIT = do
    pc <- getPC 
    let pushed_pc = pc + 1 -- Currently, PC points to the byte NEXT to the BRK instruction. But for some ill reason, the 6502 will push the byte after that one to the stack instead.
    let (pchb, pclb) = splitBytes (pushed_pc) --
    ps <- getPS 
    writeStack pchb -- Write the high byte of the PC to the stack
    writeStack pclb -- Write the low byte of the PC to the stack
    writeStack (setBit ps 4) -- Write the PS to the stack with fourth bit (B flag) set.
    irq_lb <- readByte 0xFFFE -- Get the IRQ interrupt vector
    irq_hb <- readByte 0xFFFF --
    let jmp_addr = joinBytes irq_hb irq_lb
    setPC jmp_addr -- Jump to the address
    setFlag INTERRUPT_DISABLE True -- I'm not confident this happens. TODO: Verify this.

opBVC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBVC IMPLICIT = error "Operation BVC does not support IMPLICIT addressing mode"
opBVC ACCUMULATOR = error "Operation BVC does not support ACCUMULATOR addressing mode"
opBVC IMMEDIATE = error "Operation BVC does not support IMMEDIATE addressing mode"
opBVC ZEROPAGE = error "Operation BVC does not support ZEROPAGE addressing mode"
opBVC ZEROPAGE_X = error "Operation BVC does not support ZEROPAGE_X addressing mode"
opBVC ZEROPAGE_Y = error "Operation BVC does not support ZEROPAGE_Y addressing mode"
opBVC ABSOLUTE = error "Operation BVC does not support ABSOLUTE addressing mode"
opBVC ABSOLUTE_X = error "Operation BVC does not support ABSOLUTE_X addressing mode"
opBVC ABSOLUTE_Y = error "Operation BVC does not support ABSOLUTE_Y addressing mode"
opBVC INDIRECT = error "Operation BVC does not support INDIRECT addressing mode"
opBVC INDIRECT_X = error "Operation BVC does not support INDIRECT_X addressing mode"
opBVC INDIRECT_Y = error "Operation BVC does not support INDIRECT_Y addressing mode"
opBVC RELATIVE = do
    overflow_flag <- getFlag OVERFLOW -- Get Overflow flag
    when (not overflow_flag) (updateCycles 1)
    when (not overflow_flag) (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when (not overflow_flag) (setPC addr)

opBVS ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opBVS IMPLICIT = error "Operation BVS does not support IMPLICIT addressing mode"
opBVS ACCUMULATOR = error "Operation BVS does not support ACCUMULATOR addressing mode"
opBVS IMMEDIATE = error "Operation BVS does not support IMMEDIATE addressing mode"
opBVS ZEROPAGE = error "Operation BVS does not support ZEROPAGE addressing mode"
opBVS ZEROPAGE_X = error "Operation BVS does not support ZEROPAGE_X addressing mode"
opBVS ZEROPAGE_Y = error "Operation BVS does not support ZEROPAGE_Y addressing mode"
opBVS ABSOLUTE = error "Operation BVS does not support ABSOLUTE addressing mode"
opBVS ABSOLUTE_X = error "Operation BVS does not support ABSOLUTE_X addressing mode"
opBVS ABSOLUTE_Y = error "Operation BVS does not support ABSOLUTE_Y addressing mode"
opBVS INDIRECT = error "Operation BVS does not support INDIRECT addressing mode"
opBVS INDIRECT_X = error "Operation BVS does not support INDIRECT_X addressing mode"
opBVS INDIRECT_Y = error "Operation BVS does not support INDIRECT_Y addressing mode"
opBVS RELATIVE = do
    overflow_flag <- getFlag OVERFLOW -- Get Overflow flag
    when overflow_flag (updateCycles 1)
    when overflow_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE -- Get jump address
    when overflow_flag (setPC addr)

opCLC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCLC ACCUMULATOR = error "Operation CLC does not support ACCUMULATOR addressing mode"
opCLC IMMEDIATE = error "Operation CLC does not support IMMEDIATE addressing mode"
opCLC ZEROPAGE = error "Operation CLC does not support ZEROPAGE addressing mode"
opCLC ZEROPAGE_X = error "Operation CLC does not support ZEROPAGE_X addressing mode"
opCLC ZEROPAGE_Y = error "Operation CLC does not support ZEROPAGE_Y addressing mode"
opCLC RELATIVE = error "Operation CLC does not support RELATIVE addressing mode"
opCLC ABSOLUTE = error "Operation CLC does not support ABSOLUTE addressing mode"
opCLC ABSOLUTE_X = error "Operation CLC does not support ABSOLUTE_X addressing mode"
opCLC ABSOLUTE_Y = error "Operation CLC does not support ABSOLUTE_Y addressing mode"
opCLC INDIRECT = error "Operation CLC does not support INDIRECT addressing mode"
opCLC INDIRECT_X = error "Operation CLC does not support INDIRECT_X addressing mode"
opCLC INDIRECT_Y = error "Operation CLC does not support INDIRECT_Y addressing mode"
opCLC IMPLICIT = do
    setFlag CARRY False

opCLD ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCLD ACCUMULATOR = error "Operation CLD does not support ACCUMULATOR addressing mode"
opCLD IMMEDIATE = error "Operation CLD does not support IMMEDIATE addressing mode"
opCLD ZEROPAGE = error "Operation CLD does not support ZEROPAGE addressing mode"
opCLD ZEROPAGE_X = error "Operation CLD does not support ZEROPAGE_X addressing mode"
opCLD ZEROPAGE_Y = error "Operation CLD does not support ZEROPAGE_Y addressing mode"
opCLD RELATIVE = error "Operation CLD does not support RELATIVE addressing mode"
opCLD ABSOLUTE = error "Operation CLD does not support ABSOLUTE addressing mode"
opCLD ABSOLUTE_X = error "Operation CLD does not support ABSOLUTE_X addressing mode"
opCLD ABSOLUTE_Y = error "Operation CLD does not support ABSOLUTE_Y addressing mode"
opCLD INDIRECT = error "Operation CLD does not support INDIRECT addressing mode"
opCLD INDIRECT_X = error "Operation CLD does not support INDIRECT_X addressing mode"
opCLD INDIRECT_Y = error "Operation CLD does not support INDIRECT_Y addressing mode"
opCLD IMPLICIT = do
    setFlag DECIMAL_MODE False

opCLI ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCLI ACCUMULATOR = error "Operation CLI does not support ACCUMULATOR addressing mode"
opCLI IMMEDIATE = error "Operation CLI does not support IMMEDIATE addressing mode"
opCLI ZEROPAGE = error "Operation CLI does not support ZEROPAGE addressing mode"
opCLI ZEROPAGE_X = error "Operation CLI does not support ZEROPAGE_X addressing mode"
opCLI ZEROPAGE_Y = error "Operation CLI does not support ZEROPAGE_Y addressing mode"
opCLI RELATIVE = error "Operation CLI does not support RELATIVE addressing mode"
opCLI ABSOLUTE = error "Operation CLI does not support ABSOLUTE addressing mode"
opCLI ABSOLUTE_X = error "Operation CLI does not support ABSOLUTE_X addressing mode"
opCLI ABSOLUTE_Y = error "Operation CLI does not support ABSOLUTE_Y addressing mode"
opCLI INDIRECT = error "Operation CLI does not support INDIRECT addressing mode"
opCLI INDIRECT_X = error "Operation CLI does not support INDIRECT_X addressing mode"
opCLI INDIRECT_Y = error "Operation CLI does not support INDIRECT_Y addressing mode"
opCLI IMPLICIT = do
    setFlag INTERRUPT_DISABLE False

opCLV ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCLV ACCUMULATOR = error "Operation CLV does not support ACCUMULATOR addressing mode"
opCLV IMMEDIATE = error "Operation CLV does not support IMMEDIATE addressing mode"
opCLV ZEROPAGE = error "Operation CLV does not support ZEROPAGE addressing mode"
opCLV ZEROPAGE_X = error "Operation CLV does not support ZEROPAGE_X addressing mode"
opCLV ZEROPAGE_Y = error "Operation CLV does not support ZEROPAGE_Y addressing mode"
opCLV RELATIVE = error "Operation CLV does not support RELATIVE addressing mode"
opCLV ABSOLUTE = error "Operation CLV does not support ABSOLUTE addressing mode"
opCLV ABSOLUTE_X = error "Operation CLV does not support ABSOLUTE_X addressing mode"
opCLV ABSOLUTE_Y = error "Operation CLV does not support ABSOLUTE_Y addressing mode"
opCLV INDIRECT = error "Operation CLV does not support INDIRECT addressing mode"
opCLV INDIRECT_X = error "Operation CLV does not support INDIRECT_X addressing mode"
opCLV INDIRECT_Y = error "Operation CLV does not support INDIRECT_Y addressing mode"
opCLV IMPLICIT = do
    setFlag OVERFLOW False

opCMP ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCMP IMPLICIT = error "Operation CMP does not support IMPLICIT addressing mode"
opCMP ACCUMULATOR = error "Operation CMP does not support ACCUMULATOR addressing mode"
opCMP ZEROPAGE_Y = error "Operation CMP does not support ZEROPAGE_Y addressing mode"
opCMP RELATIVE = error "Operation CMP does not support RELATIVE addressing mode"
opCMP INDIRECT = error "Operation CMP does not support INDIRECT addressing mode"
opCMP addr_mode = do
    acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = acc - byte -- Compares the accumulator with a memory value
    setFlag ZERO (acc == byte) -- Set the Zero flag if they are equal
    setFlag CARRY (acc >= byte) -- Set the Carry flag if Acc >= mem_value
    setFlag NEGATIVE (b7' result) -- Set the Negative flag if

opCPX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCPX IMPLICIT = error "Operation CPX does not support IMPLICIT addressing mode"
opCPX ACCUMULATOR = error "Operation CPX does not support ACCUMULATOR addressing mode"
opCPX ZEROPAGE_X = error "Operation CPX does not support ZEROPAGE_X addressing mode"
opCPX ZEROPAGE_Y = error "Operation CPX does not support ZEROPAGE_Y addressing mode"
opCPX RELATIVE = error "Operation CPX does not support RELATIVE addressing mode"
opCPX ABSOLUTE_X = error "Operation CPX does not support ABSOLUTE_X addressing mode"
opCPX ABSOLUTE_Y = error "Operation CPX does not support ABSOLUTE_Y addressing mode"
opCPX INDIRECT = error "Operation CPX does not support INDIRECT addressing mode"
opCPX INDIRECT_X = error "Operation CPX does not support INDIRECT_X addressing mode"
opCPX INDIRECT_Y = error "Operation CPX does not support INDIRECT_Y addressing mode"
opCPX addr_mode = do
    xreg <- getIDX 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = xreg - byte -- Compares the accumulator with a memory value
    setFlag ZERO (xreg == byte) -- Set the Zero flag if they are equal
    setFlag CARRY (xreg >= byte) -- Set the Carry flag if Acc >= mem_value
    setFlag NEGATIVE (b7' result) -- Set the Negative flag if

opCPY ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opCPY IMPLICIT = error "Operation CPY does not support IMPLICIT addressing mode"
opCPY ACCUMULATOR = error "Operation CPY does not support ACCUMULATOR addressing mode"
opCPY ZEROPAGE_X = error "Operation CPY does not support ZEROPAGE_X addressing mode"
opCPY ZEROPAGE_Y = error "Operation CPY does not support ZEROPAGE_Y addressing mode"
opCPY RELATIVE = error "Operation CPY does not support RELATIVE addressing mode"
opCPY ABSOLUTE_X = error "Operation CPY does not support ABSOLUTE_X addressing mode"
opCPY ABSOLUTE_Y = error "Operation CPY does not support ABSOLUTE_Y addressing mode"
opCPY INDIRECT = error "Operation CPY does not support INDIRECT addressing mode"
opCPY INDIRECT_X = error "Operation CPY does not support INDIRECT_X addressing mode"
opCPY INDIRECT_Y = error "Operation CPY does not support INDIRECT_Y addressing mode"
opCPY addr_mode = do
    yreg <- getIDY 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = yreg - byte -- Compares the accumulator with a memory value
    setFlag ZERO (yreg == byte) -- Set the Zero flag if they are equal
    setFlag CARRY (yreg >= byte) -- Set the Carry flag if Acc >= mem_value
    setFlag NEGATIVE (b7' result) -- Set the Negative flag if

opDEC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opDEC IMPLICIT = error "Operation DEC does not support IMPLICIT addressing mode"
opDEC ACCUMULATOR = error "Operation DEC does not support ACCUMULATOR addressing mode"
opDEC IMMEDIATE = error "Operation DEC does not support IMMEDIATE addressing mode"
opDEC ZEROPAGE_Y = error "Operation DEC does not support ZEROPAGE_Y addressing mode"
opDEC RELATIVE = error "Operation DEC does not support RELATIVE addressing mode"
opDEC ABSOLUTE_Y = error "Operation DEC does not support ABSOLUTE_Y addressing mode"
opDEC INDIRECT = error "Operation DEC does not support INDIRECT addressing mode"
opDEC INDIRECT_X = error "Operation DEC does not support INDIRECT_X addressing mode"
opDEC INDIRECT_Y = error "Operation DEC does not support INDIRECT_Y addressing mode"
opDEC addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = byte - 1
    writeByte addr result
    setFlag ZERO (result == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7' result) -- Sets the Negative flag is the result is negative

opDEX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opDEX ACCUMULATOR = error "Operation DEX does not support ACCUMULATOR addressing mode"
opDEX IMMEDIATE = error "Operation DEX does not support IMMEDIATE addressing mode"
opDEX ZEROPAGE = error "Operation DEX does not support ZEROPAGE addressing mode"
opDEX ZEROPAGE_X = error "Operation DEX does not support ZEROPAGE_X addressing mode"
opDEX ZEROPAGE_Y = error "Operation DEX does not support ZEROPAGE_Y addressing mode"
opDEX RELATIVE = error "Operation DEX does not support RELATIVE addressing mode"
opDEX ABSOLUTE = error "Operation DEX does not support ABSOLUTE addressing mode"
opDEX ABSOLUTE_X = error "Operation DEX does not support ABSOLUTE_X addressing mode"
opDEX ABSOLUTE_Y = error "Operation DEX does not support ABSOLUTE_Y addressing mode"
opDEX INDIRECT = error "Operation DEX does not support INDIRECT addressing mode"
opDEX INDIRECT_X = error "Operation DEX does not support INDIRECT_X addressing mode"
opDEX INDIRECT_Y = error "Operation DEX does not support INDIRECT_Y addressing mode"
opDEX IMPLICIT = do
    mapIDX (\x -> x - (1 :: Word8)) -- Increases the X Register by one
    idx <- getIDX 
    setFlag ZERO (idx == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7' idx) -- Sets the Negative flag is the result is negative

opDEY ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opDEY ACCUMULATOR = error "Operation DEY does not support ACCUMULATOR addressing mode"
opDEY IMMEDIATE = error "Operation DEY does not support IMMEDIATE addressing mode"
opDEY ZEROPAGE = error "Operation DEY does not support ZEROPAGE addressing mode"
opDEY ZEROPAGE_X = error "Operation DEY does not support ZEROPAGE_X addressing mode"
opDEY ZEROPAGE_Y = error "Operation DEY does not support ZEROPAGE_Y addressing mode"
opDEY RELATIVE = error "Operation DEY does not support RELATIVE addressing mode"
opDEY ABSOLUTE = error "Operation DEY does not support ABSOLUTE addressing mode"
opDEY ABSOLUTE_X = error "Operation DEY does not support ABSOLUTE_X addressing mode"
opDEY ABSOLUTE_Y = error "Operation DEY does not support ABSOLUTE_Y addressing mode"
opDEY INDIRECT = error "Operation DEY does not support INDIRECT addressing mode"
opDEY INDIRECT_X = error "Operation DEY does not support INDIRECT_X addressing mode"
opDEY INDIRECT_Y = error "Operation DEY does not support INDIRECT_Y addressing mode"
opDEY IMPLICIT = do
    mapIDY (\x -> x - (1 :: Word8)) -- Increases the X Register by one
    idy <- getIDY 
    setFlag ZERO (idy == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7' idy) -- Sets the Negative flag is the result is negative

opEOR ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opEOR IMPLICIT = error "Operation EOR does not support IMPLICIT addressing mode"
opEOR ACCUMULATOR = error "Operation EOR does not support ACCUMULATOR addressing mode"
opEOR ZEROPAGE_Y = error "Operation EOR does not support ZEROPAGE_Y addressing mode"
opEOR RELATIVE = error "Operation EOR does not support RELATIVE addressing mode"
opEOR INDIRECT = error "Operation EOR does not support INDIRECT addressing mode"
opEOR addr_mode = do
    old_acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    mapACC (`xor` byte) -- XOR the corresponding byte to Accumulator
    acc <- getACC 
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' acc) -- Sets the Negative flag is the result is negative

opINC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opINC IMPLICIT = error "Operation INC does not support IMPLICIT addressing mode"
opINC ACCUMULATOR = error "Operation INC does not support ACCUMULATOR addressing mode"
opINC IMMEDIATE = error "Operation INC does not support IMMEDIATE addressing mode"
opINC ZEROPAGE_Y = error "Operation INC does not support ZEROPAGE_Y addressing mode"
opINC RELATIVE = error "Operation INC does not support RELATIVE addressing mode"
opINC ABSOLUTE_Y = error "Operation INC does not support ABSOLUTE_Y addressing mode"
opINC INDIRECT = error "Operation INC does not support INDIRECT addressing mode"
opINC INDIRECT_X = error "Operation INC does not support INDIRECT_X addressing mode"
opINC INDIRECT_Y = error "Operation INC does not support INDIRECT_Y addressing mode"
opINC addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = byte + 1
    writeByte addr result
    setFlag ZERO (result == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7' result) -- Sets the Negative flag is the result is negative

opINX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opINX ACCUMULATOR = error "Operation INX does not support ACCUMULATOR addressing mode"
opINX IMMEDIATE = error "Operation INX does not support IMMEDIATE addressing mode"
opINX ZEROPAGE = error "Operation INX does not support ZEROPAGE addressing mode"
opINX ZEROPAGE_X = error "Operation INX does not support ZEROPAGE_X addressing mode"
opINX ZEROPAGE_Y = error "Operation INX does not support ZEROPAGE_Y addressing mode"
opINX RELATIVE = error "Operation INX does not support RELATIVE addressing mode"
opINX ABSOLUTE = error "Operation INX does not support ABSOLUTE addressing mode"
opINX ABSOLUTE_X = error "Operation INX does not support ABSOLUTE_X addressing mode"
opINX ABSOLUTE_Y = error "Operation INX does not support ABSOLUTE_Y addressing mode"
opINX INDIRECT = error "Operation INX does not support INDIRECT addressing mode"
opINX INDIRECT_X = error "Operation INX does not support INDIRECT_X addressing mode"
opINX INDIRECT_Y = error "Operation INX does not support INDIRECT_Y addressing mode"
opINX IMPLICIT = do
    mapIDX (+ (1 :: Word8)) -- Increases the X Register by one
    idx <- getIDX 
    setFlag ZERO (idx == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7' idx) -- Sets the Negative flag is the result is negative

opINY ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opINY ACCUMULATOR = error "Operation INY does not support ACCUMULATOR addressing mode"
opINY IMMEDIATE = error "Operation INY does not support IMMEDIATE addressing mode"
opINY ZEROPAGE = error "Operation INY does not support ZEROPAGE addressing mode"
opINY ZEROPAGE_X = error "Operation INY does not support ZEROPAGE_X addressing mode"
opINY ZEROPAGE_Y = error "Operation INY does not support ZEROPAGE_Y addressing mode"
opINY RELATIVE = error "Operation INY does not support RELATIVE addressing mode"
opINY ABSOLUTE = error "Operation INY does not support ABSOLUTE addressing mode"
opINY ABSOLUTE_X = error "Operation INY does not support ABSOLUTE_X addressing mode"
opINY ABSOLUTE_Y = error "Operation INY does not support ABSOLUTE_Y addressing mode"
opINY INDIRECT = error "Operation INY does not support INDIRECT addressing mode"
opINY INDIRECT_X = error "Operation INY does not support INDIRECT_X addressing mode"
opINY INDIRECT_Y = error "Operation INY does not support INDIRECT_Y addressing mode"
opINY IMPLICIT = do
    mapIDY (+ (1 :: Word8)) -- Increases the X Register by one
    idy <- getIDY 
    setFlag ZERO (idy == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7' idy) -- Sets the Negative flag is the result is negative

opJMP ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opJMP IMPLICIT = error "Operation JMP does not support IMPLICIT addressing mode"
opJMP ACCUMULATOR = error "Operation JMP does not support ACCUMULATOR addressing mode"
opJMP IMMEDIATE = error "Operation JMP does not support IMMEDIATE addressing mode"
opJMP ZEROPAGE = error "Operation JMP does not support ZEROPAGE addressing mode"
opJMP ZEROPAGE_X = error "Operation JMP does not support ZEROPAGE_X addressing mode"
opJMP ZEROPAGE_Y = error "Operation JMP does not support ZEROPAGE_Y addressing mode"
opJMP RELATIVE = error "Operation JMP does not support RELATIVE addressing mode"
opJMP ABSOLUTE_X = error "Operation JMP does not support ABSOLUTE_X addressing mode"
opJMP ABSOLUTE_Y = error "Operation JMP does not support ABSOLUTE_Y addressing mode"
opJMP INDIRECT_X = error "Operation JMP does not support INDIRECT_X addressing mode"
opJMP INDIRECT_Y = error "Operation JMP does not support INDIRECT_Y addressing mode"
opJMP addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    setPC addr

opJSR ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opJSR IMPLICIT = error "Operation JSR does not support IMPLICIT addressing mode"
opJSR ACCUMULATOR = error "Operation JSR does not support ACCUMULATOR addressing mode"
opJSR IMMEDIATE = error "Operation JSR does not support IMMEDIATE addressing mode"
opJSR ZEROPAGE = error "Operation JSR does not support ZEROPAGE addressing mode"
opJSR ZEROPAGE_X = error "Operation JSR does not support ZEROPAGE_X addressing mode"
opJSR ZEROPAGE_Y = error "Operation JSR does not support ZEROPAGE_Y addressing mode"
opJSR RELATIVE = error "Operation JSR does not support RELATIVE addressing mode"
opJSR ABSOLUTE_X = error "Operation JSR does not support ABSOLUTE_X addressing mode"
opJSR ABSOLUTE_Y = error "Operation JSR does not support ABSOLUTE_Y addressing mode"
opJSR INDIRECT = error "Operation JSR does not support INDIRECT addressing mode"
opJSR INDIRECT_X = error "Operation JSR does not support INDIRECT_X addressing mode"
opJSR INDIRECT_Y = error "Operation JSR does not support INDIRECT_Y addressing mode"
opJSR addr_mode = do
    -- There is a peculiarity in the MOS 6502 where JSR will
    -- instead of pushing the position of the next address to stack, it will
    -- push the location prior to that address instead.
    pc <- getPC 
    let (hb, lb) = splitBytes (pc + 1)
    writeStack hb
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    writeStack lb
    setPC addr

opLDA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opLDA IMPLICIT = error "Operation LDA does not support IMPLICIT addressing mode"
opLDA ACCUMULATOR = error "Operation LDA does not support ACCUMULATOR addressing mode"
opLDA ZEROPAGE_Y = error "Operation LDA does not support ZEROPAGE_Y addressing mode"
opLDA RELATIVE = error "Operation LDA does not support RELATIVE addressing mode"
opLDA INDIRECT = error "Operation LDA does not support INDIRECT addressing mode"
opLDA addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr
    setACC byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7' byte)

opLDX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opLDX IMPLICIT = error "Operation LDX does not support IMPLICIT addressing mode"
opLDX ACCUMULATOR = error "Operation LDX does not support ACCUMULATOR addressing mode"
opLDX ZEROPAGE_X = error "Operation LDX does not support ZEROPAGE_X addressing mode"
opLDX RELATIVE = error "Operation LDX does not support RELATIVE addressing mode"
opLDX ABSOLUTE_X = error "Operation LDX does not support ABSOLUTE_X addressing mode"
opLDX INDIRECT = error "Operation LDX does not support INDIRECT addressing mode"
opLDX INDIRECT_X = error "Operation LDX does not support INDIRECT_X addressing mode"
opLDX INDIRECT_Y = error "Operation LDX does not support INDIRECT_Y addressing mode"
opLDX addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr
    setIDX byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7' byte)

opLDY ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opLDY IMPLICIT = error "Operation LDY does not support IMPLICIT addressing mode"
opLDY ACCUMULATOR = error "Operation LDY does not support ACCUMULATOR addressing mode"
opLDY ZEROPAGE_Y = error "Operation LDY does not support ZEROPAGE_X addressing mode"
opLDY RELATIVE = error "Operation LDY does not support RELATIVE addressing mode"
opLDY ABSOLUTE_Y = error "Operation LDY does not support ABSOLUTE_X addressing mode"
opLDY INDIRECT = error "Operation LDY does not support INDIRECT addressing mode"
opLDY INDIRECT_X = error "Operation LDY does not support INDIRECT_X addressing mode"
opLDY INDIRECT_Y = error "Operation LDY does not support INDIRECT_Y addressing mode"
opLDY addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr
    setIDY byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7' byte)

opLSR ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opLSR IMPLICIT = error "Operation LSR does not support IMPLICIT addressing mode"
opLSR IMMEDIATE = error "Operation LSR does not support IMMEDIATE addressing mode"
opLSR ZEROPAGE_Y = error "Operation LSR does not support ZEROPAGE_Y addressing mode"
opLSR RELATIVE = error "Operation LSR does not support RELATIVE addressing mode"
opLSR ABSOLUTE_Y = error "Operation LSR does not support ABSOLUTE_Y addressing mode"
opLSR INDIRECT = error "Operation LSR does not support INDIRECT addressing mode"
opLSR INDIRECT_X = error "Operation LSR does not support INDIRECT_X addressing mode"
opLSR INDIRECT_Y = error "Operation LSR does not support INDIRECT_Y addressing mode"
opLSR ACCUMULATOR = do
    old_acc <- getACC 
    let carry_flag = b0' old_acc -- Carry flag is set to contents of old bit 0
    mapACC ((\x -> x .>>. 1) :: Word8 -> Word8)
    acc <- getACC 
    setFlag CARRY carry_flag
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' acc) -- Sets the Negative flag is the result is negative
opLSR addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    let carry_flag = b0' byte -- Carry flag is set to contents of old bit 0
    let new_byte = byte .>>. 1 :: Word8
    writeByte addr new_byte
    setFlag CARRY carry_flag
    setFlag ZERO (new_byte == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' new_byte) -- Sets the Negative flag is the result is negative

opNOP ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opNOP ACCUMULATOR = error "Operation NOP does not support ACCUMULATOR addressing mode"
opNOP IMMEDIATE = error "Operation NOP does not support IMMEDIATE addressing mode"
opNOP ZEROPAGE = error "Operation NOP does not support ZEROPAGE addressing mode"
opNOP ZEROPAGE_X = error "Operation NOP does not support ZEROPAGE_X addressing mode"
opNOP ZEROPAGE_Y = error "Operation NOP does not support ZEROPAGE_Y addressing mode"
opNOP RELATIVE = error "Operation NOP does not support RELATIVE addressing mode"
opNOP ABSOLUTE = error "Operation NOP does not support ABSOLUTE addressing mode"
opNOP ABSOLUTE_X = error "Operation NOP does not support ABSOLUTE_X addressing mode"
opNOP ABSOLUTE_Y = error "Operation NOP does not support ABSOLUTE_Y addressing mode"
opNOP INDIRECT = error "Operation NOP does not support INDIRECT addressing mode"
opNOP INDIRECT_X = error "Operation NOP does not support INDIRECT_X addressing mode"
opNOP INDIRECT_Y = error "Operation NOP does not support INDIRECT_Y addressing mode"
opNOP IMPLICIT = return ()

opORA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opORA IMPLICIT = error "Operation ORA does not support IMPLICIT addressing mode"
opORA ACCUMULATOR = error "Operation ORA does not support ACCUMULATOR addressing mode"
opORA ZEROPAGE_Y = error "Operation ORA does not support ZEROPAGE_Y addressing mode"
opORA RELATIVE = error "Operation ORA does not support RELATIVE addressing mode"
opORA INDIRECT = error "Operation ORA does not support INDIRECT addressing mode"
opORA addr_mode = do
    old_acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    mapACC (.|. byte) -- OR the corresponding byte to Accumulator
    acc <- getACC 
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7' acc) -- Sets the Negative flag is the result is negative

opPHA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opPHA ACCUMULATOR = error "Operation PHA does not support ACCUMULATOR addressing mode"
opPHA IMMEDIATE = error "Operation PHA does not support IMMEDIATE addressing mode"
opPHA ZEROPAGE = error "Operation PHA does not support ZEROPAGE addressing mode"
opPHA ZEROPAGE_X = error "Operation PHA does not support ZEROPAGE_X addressing mode"
opPHA ZEROPAGE_Y = error "Operation PHA does not support ZEROPAGE_Y addressing mode"
opPHA RELATIVE = error "Operation PHA does not support RELATIVE addressing mode"
opPHA ABSOLUTE = error "Operation PHA does not support ABSOLUTE addressing mode"
opPHA ABSOLUTE_X = error "Operation PHA does not support ABSOLUTE_X addressing mode"
opPHA ABSOLUTE_Y = error "Operation PHA does not support ABSOLUTE_Y addressing mode"
opPHA INDIRECT = error "Operation PHA does not support INDIRECT addressing mode"
opPHA INDIRECT_X = error "Operation PHA does not support INDIRECT_X addressing mode"
opPHA INDIRECT_Y = error "Operation PHA does not support INDIRECT_Y addressing mode"
opPHA IMPLICIT = do
    acc <- getACC 
    writeStack acc

opPHP ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opPHP ACCUMULATOR = error "Operation PHP does not support ACCUMULATOR addressing mode"
opPHP IMMEDIATE = error "Operation PHP does not support IMMEDIATE addressing mode"
opPHP ZEROPAGE = error "Operation PHP does not support ZEROPAGE addressing mode"
opPHP ZEROPAGE_X = error "Operation PHP does not support ZEROPAGE_X addressing mode"
opPHP ZEROPAGE_Y = error "Operation PHP does not support ZEROPAGE_Y addressing mode"
opPHP RELATIVE = error "Operation PHP does not support RELATIVE addressing mode"
opPHP ABSOLUTE = error "Operation PHP does not support ABSOLUTE addressing mode"
opPHP ABSOLUTE_X = error "Operation PHP does not support ABSOLUTE_X addressing mode"
opPHP ABSOLUTE_Y = error "Operation PHP does not support ABSOLUTE_Y addressing mode"
opPHP INDIRECT = error "Operation PHP does not support INDIRECT addressing mode"
opPHP INDIRECT_X = error "Operation PHP does not support INDIRECT_X addressing mode"
opPHP INDIRECT_Y = error "Operation PHP does not support INDIRECT_Y addressing mode"
opPHP IMPLICIT = do
    ps <- getPS 
    writeStack (setBit ps 4)

opPLA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opPLA ACCUMULATOR = error "Operation PLA does not support ACCUMULATOR addressing mode"
opPLA IMMEDIATE = error "Operation PLA does not support IMMEDIATE addressing mode"
opPLA ZEROPAGE = error "Operation PLA does not support ZEROPAGE addressing mode"
opPLA ZEROPAGE_X = error "Operation PLA does not support ZEROPAGE_X addressing mode"
opPLA ZEROPAGE_Y = error "Operation PLA does not support ZEROPAGE_Y addressing mode"
opPLA RELATIVE = error "Operation PLA does not support RELATIVE addressing mode"
opPLA ABSOLUTE = error "Operation PLA does not support ABSOLUTE addressing mode"
opPLA ABSOLUTE_X = error "Operation PLA does not support ABSOLUTE_X addressing mode"
opPLA ABSOLUTE_Y = error "Operation PLA does not support ABSOLUTE_Y addressing mode"
opPLA INDIRECT = error "Operation PLA does not support INDIRECT addressing mode"
opPLA INDIRECT_X = error "Operation PLA does not support INDIRECT_X addressing mode"
opPLA INDIRECT_Y = error "Operation PLA does not support INDIRECT_Y addressing mode"
opPLA IMPLICIT = do
    acc <- readStack
    setACC acc
    setFlag ZERO (acc == 0)
    setFlag NEGATIVE (b7' acc)

opPLP ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opPLP ACCUMULATOR = error "Operation PLP does not support ACCUMULATOR addressing mode"
opPLP IMMEDIATE = error "Operation PLP does not support IMMEDIATE addressing mode"
opPLP ZEROPAGE = error "Operation PLP does not support ZEROPAGE addressing mode"
opPLP ZEROPAGE_X = error "Operation PLP does not support ZEROPAGE_X addressing mode"
opPLP ZEROPAGE_Y = error "Operation PLP does not support ZEROPAGE_Y addressing mode"
opPLP RELATIVE = error "Operation PLP does not support RELATIVE addressing mode"
opPLP ABSOLUTE = error "Operation PLP does not support ABSOLUTE addressing mode"
opPLP ABSOLUTE_X = error "Operation PLP does not support ABSOLUTE_X addressing mode"
opPLP ABSOLUTE_Y = error "Operation PLP does not support ABSOLUTE_Y addressing mode"
opPLP INDIRECT = error "Operation PLP does not support INDIRECT addressing mode"
opPLP INDIRECT_X = error "Operation PLP does not support INDIRECT_X addressing mode"
opPLP INDIRECT_Y = error "Operation PLP does not support INDIRECT_Y addressing mode"
opPLP IMPLICIT = do
    ps <- readStack
    let ps' = setBit ps 5
    let ps'' = clearBit ps' 4
    setPS ps''

opROL ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opROL IMPLICIT = error "Operation ROL does not support IMPLICIT addressing mode"
opROL IMMEDIATE = error "Operation ROL does not support IMMEDIATE addressing mode"
opROL ZEROPAGE_Y = error "Operation ROL does not support ZEROPAGE_Y addressing mode"
opROL RELATIVE = error "Operation ROL does not support RELATIVE addressing mode"
opROL ABSOLUTE_Y = error "Operation ROL does not support ABSOLUTE_Y addressing mode"
opROL INDIRECT = error "Operation ROL does not support INDIRECT addressing mode"
opROL INDIRECT_X = error "Operation ROL does not support INDIRECT_X addressing mode"
opROL INDIRECT_Y = error "Operation ROL does not support INDIRECT_Y addressing mode"
opROL ACCUMULATOR = do
    acc <- getACC 
    carry_flag <- getFlag CARRY
    let new_carry = b7' acc
    let bit0 = if carry_flag then (0x01 :: Word8) else (0x00 :: Word8)
    let acc' = (acc .<<. 1) .|. bit0
    setACC acc'
    setFlag ZERO (acc' == 0)
    setFlag NEGATIVE (b7' acc')
    setFlag CARRY new_carry
opROL addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    carry_flag <- getFlag CARRY
    let new_carry = b7' byte
    let bit0 = if carry_flag then (0x01 :: Word8) else (0x00 :: Word8)
    let byte' = (byte .<<. 1) .|. bit0
    writeByte addr byte'
    setFlag ZERO (byte' == 0)
    setFlag NEGATIVE (b7' byte')
    setFlag CARRY new_carry

opROR ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opROR IMPLICIT = error "Operation ROR does not support IMPLICIT addressing mode"
opROR IMMEDIATE = error "Operation ROR does not support IMMEDIATE addressing mode"
opROR ZEROPAGE_Y = error "Operation ROR does not support ZEROPAGE_Y addressing mode"
opROR RELATIVE = error "Operation ROR does not support RELATIVE addressing mode"
opROR ABSOLUTE_Y = error "Operation ROR does not support ABSOLUTE_Y addressing mode"
opROR INDIRECT = error "Operation ROR does not support INDIRECT addressing mode"
opROR INDIRECT_X = error "Operation ROR does not support INDIRECT_X addressing mode"
opROR INDIRECT_Y = error "Operation ROR does not support INDIRECT_Y addressing mode"
opROR ACCUMULATOR = do
    acc <- getACC 
    carry_flag <- getFlag CARRY
    let new_carry = b0' acc
    let bit0 = if carry_flag then (0x80 :: Word8) else (0x00 :: Word8)
    let acc' = (acc .>>. 1) .|. bit0
    setACC acc'
    setFlag ZERO (acc' == 0)
    setFlag NEGATIVE (b7' acc')
    setFlag CARRY new_carry
opROR addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    carry_flag <- getFlag CARRY
    let new_carry = b0' byte
    let bit0 = if carry_flag then (0x80 :: Word8) else (0x00 :: Word8)
    let byte' = (byte .>>. 1) .|. bit0
    writeByte addr byte'
    setFlag ZERO (byte' == 0)
    setFlag NEGATIVE (b7' byte')
    setFlag CARRY new_carry

opRTI ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opRTI ACCUMULATOR = error "Operation RTI does not support ACCUMULATOR addressing mode"
opRTI IMMEDIATE = error "Operation RTI does not support IMMEDIATE addressing mode"
opRTI ZEROPAGE = error "Operation RTI does not support ZEROPAGE addressing mode"
opRTI ZEROPAGE_X = error "Operation RTI does not support ZEROPAGE_X addressing mode"
opRTI ZEROPAGE_Y = error "Operation RTI does not support ZEROPAGE_Y addressing mode"
opRTI RELATIVE = error "Operation RTI does not support RELATIVE addressing mode"
opRTI ABSOLUTE = error "Operation RTI does not support ABSOLUTE addressing mode"
opRTI ABSOLUTE_X = error "Operation RTI does not support ABSOLUTE_X addressing mode"
opRTI ABSOLUTE_Y = error "Operation RTI does not support ABSOLUTE_Y addressing mode"
opRTI INDIRECT = error "Operation RTI does not support INDIRECT addressing mode"
opRTI INDIRECT_X = error "Operation RTI does not support INDIRECT_X addressing mode"
opRTI INDIRECT_Y = error "Operation RTI does not support INDIRECT_Y addressing mode"
opRTI IMPLICIT = do
    ps <- readStack
    let ps' = setBit ps 5
    let ps'' = clearBit ps' 4

    pclb <- readStack
    pchb <- readStack
    let pc = joinBytes pchb pclb
    setPS ps''
    setPC pc

opRTS ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opRTS ACCUMULATOR = error "Operation RTS does not support ACCUMULATOR addressing mode"
opRTS IMMEDIATE = error "Operation RTS does not support IMMEDIATE addressing mode"
opRTS ZEROPAGE = error "Operation RTS does not support ZEROPAGE addressing mode"
opRTS ZEROPAGE_X = error "Operation RTS does not support ZEROPAGE_X addressing mode"
opRTS ZEROPAGE_Y = error "Operation RTS does not support ZEROPAGE_Y addressing mode"
opRTS RELATIVE = error "Operation RTS does not support RELATIVE addressing mode"
opRTS ABSOLUTE = error "Operation RTS does not support ABSOLUTE addressing mode"
opRTS ABSOLUTE_X = error "Operation RTS does not support ABSOLUTE_X addressing mode"
opRTS ABSOLUTE_Y = error "Operation RTS does not support ABSOLUTE_Y addressing mode"
opRTS INDIRECT = error "Operation RTS does not support INDIRECT addressing mode"
opRTS INDIRECT_X = error "Operation RTS does not support INDIRECT_X addressing mode"
opRTS INDIRECT_Y = error "Operation RTS does not support INDIRECT_Y addressing mode"
opRTS IMPLICIT = do
    pclb <- readStack
    pchb <- readStack
    let pc = (joinBytes pchb pclb) + 1 -- Read JSR to understand this addition
    setPC pc

opSBC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSBC IMPLICIT = error "Operation SBC does not support IMPLICIT addressing mode"
opSBC ACCUMULATOR = error "Operation SBC does not support ACCUMULATOR addressing mode"
opSBC ZEROPAGE_Y = error "Operation SBC does not support ZEROPAGE_Y addressing mode"
opSBC RELATIVE = error "Operation SBC does not support RELATIVE addressing mode"
opSBC INDIRECT = error "Operation SBC does not support INDIRECT addressing mode"
opSBC addr_mode = do
    acc <- getACC 
    carry_flag <- getFlag $ CARRY -- Get the Accumulator registers prior to changes
    decimal_flag <- getFlag $ DECIMAL_MODE
    decMode <- getDecMode
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let carry = if carry_flag then 1 else 0 :: Word8
    let iacc = fromIntegral acc :: Int
    let ibyte = fromIntegral byte :: Int
    let icarry = fromIntegral carry :: Int
    let iresult = iacc - ibyte - (1 - icarry)
    setFlag ZERO (iresult == 0)
    if decimal_flag && decMode
        then do
            let not_carry = carry `xor` 0x1
            let acc16 = fromIntegral acc :: Word16
            let operand16 = fromIntegral byte 
            let nc16 = fromIntegral not_carry :: Word16
            let decimal_result = acc16 - operand16 - nc16

            let temp161 = (acc16 .&. 0xf) - (operand16 .&. 0xf) - nc16
            let temp162 = if temp161 > 0xf then temp161 - 0x6 else temp161
            let tempadd = if temp162 > 0x0f then 0xfff0 else 0x00
            let temp163 = (temp162 .&. 0x0f) + tempadd
            let temp164 = temp163 + (acc16 .&. 0xf0) - (operand16 .&. 0xf0)
            let temp165 = if temp164 > 0xff then temp164 - 0x60 else temp164
            let r = fromIntegral temp165 :: Word8
            let ovf = ((decimal_result `xor` acc16) .&. (complement (decimal_result `xor` operand16))) .&. 0x80
            let ovf2 = ovf .>>. 1
            setFlag OVERFLOW (ovf2 /= 0)
            setFlag CARRY (not (temp164 > 0xFF))
            setFlag NEGATIVE (b7 decimal_result)
            setFlag ZERO (decimal_result .&. 0xFF == 0)
            setACC r
        else do
            let operand = ibyte `xor` 0x00FF
            let temp = iacc + operand + icarry
            setFlag NEGATIVE (bi7 temp)
            setFlag CARRY (temp .&. 0xFF00 /= 0)
            setFlag OVERFLOW (bi7 ((temp `xor` iacc) .&. (temp `xor` operand)))
            let acc' = fromIntegral (temp .&. 0xFF) :: Word8
            setACC acc'
            return ()

opSEC ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSEC ACCUMULATOR = error "Operation SEC does not support ACCUMULATOR addressing mode"
opSEC IMMEDIATE = error "Operation SEC does not support IMMEDIATE addressing mode"
opSEC ZEROPAGE = error "Operation SEC does not support ZEROPAGE addressing mode"
opSEC ZEROPAGE_X = error "Operation SEC does not support ZEROPAGE_X addressing mode"
opSEC ZEROPAGE_Y = error "Operation SEC does not support ZEROPAGE_Y addressing mode"
opSEC RELATIVE = error "Operation SEC does not support RELATIVE addressing mode"
opSEC ABSOLUTE = error "Operation SEC does not support ABSOLUTE addressing mode"
opSEC ABSOLUTE_X = error "Operation SEC does not support ABSOLUTE_X addressing mode"
opSEC ABSOLUTE_Y = error "Operation SEC does not support ABSOLUTE_Y addressing mode"
opSEC INDIRECT = error "Operation SEC does not support INDIRECT addressing mode"
opSEC INDIRECT_X = error "Operation SEC does not support INDIRECT_X addressing mode"
opSEC INDIRECT_Y = error "Operation SEC does not support INDIRECT_Y addressing mode"
opSEC IMPLICIT = do
    setFlag CARRY True

opSED ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSED ACCUMULATOR = error "Operation SED does not support ACCUMULATOR addressing mode"
opSED IMMEDIATE = error "Operation SED does not support IMMEDIATE addressing mode"
opSED ZEROPAGE = error "Operation SED does not support ZEROPAGE addressing mode"
opSED ZEROPAGE_X = error "Operation SED does not support ZEROPAGE_X addressing mode"
opSED ZEROPAGE_Y = error "Operation SED does not support ZEROPAGE_Y addressing mode"
opSED RELATIVE = error "Operation SED does not support RELATIVE addressing mode"
opSED ABSOLUTE = error "Operation SED does not support ABSOLUTE addressing mode"
opSED ABSOLUTE_X = error "Operation SED does not support ABSOLUTE_X addressing mode"
opSED ABSOLUTE_Y = error "Operation SED does not support ABSOLUTE_Y addressing mode"
opSED INDIRECT = error "Operation SED does not support INDIRECT addressing mode"
opSED INDIRECT_X = error "Operation SED does not support INDIRECT_X addressing mode"
opSED INDIRECT_Y = error "Operation SED does not support INDIRECT_Y addressing mode"
opSED IMPLICIT = do
    setFlag DECIMAL_MODE True

opSEI ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSEI ACCUMULATOR = error "Operation SEI does not support ACCUMULATOR addressing mode"
opSEI IMMEDIATE = error "Operation SEI does not support IMMEDIATE addressing mode"
opSEI ZEROPAGE = error "Operation SEI does not support ZEROPAGE addressing mode"
opSEI ZEROPAGE_X = error "Operation SEI does not support ZEROPAGE_X addressing mode"
opSEI ZEROPAGE_Y = error "Operation SEI does not support ZEROPAGE_Y addressing mode"
opSEI RELATIVE = error "Operation SEI does not support RELATIVE addressing mode"
opSEI ABSOLUTE = error "Operation SEI does not support ABSOLUTE addressing mode"
opSEI ABSOLUTE_X = error "Operation SEI does not support ABSOLUTE_X addressing mode"
opSEI ABSOLUTE_Y = error "Operation SEI does not support ABSOLUTE_Y addressing mode"
opSEI INDIRECT = error "Operation SEI does not support INDIRECT addressing mode"
opSEI INDIRECT_X = error "Operation SEI does not support INDIRECT_X addressing mode"
opSEI INDIRECT_Y = error "Operation SEI does not support INDIRECT_Y addressing mode"
opSEI IMPLICIT = do
    setFlag INTERRUPT_DISABLE True

opSTA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSTA IMPLICIT = error "Operation STA does not support IMPLICIT addressing mode"
opSTA ACCUMULATOR = error "Operation STA does not support ACCUMULATOR addressing mode"
opSTA IMMEDIATE = error "Operation STA does not support IMMEDIATE addressing mode"
opSTA ZEROPAGE_Y = error "Operation STA does not support ZEROPAGE_Y addressing mode"
opSTA RELATIVE = error "Operation STA does not support RELATIVE addressing mode"
opSTA INDIRECT = error "Operation STA does not support INDIRECT addressing mode"
opSTA addr_mode = do
    addr <- getAddr addr_mode
    acc <- getACC 
    writeByte addr acc

opSTX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSTX IMPLICIT = error "Operation STX does not support IMPLICIT addressing mode"
opSTX ACCUMULATOR = error "Operation STX does not support ACCUMULATOR addressing mode"
opSTX IMMEDIATE = error "Operation STX does not support IMMEDIATE addressing mode"
opSTX ZEROPAGE_X = error "Operation STX does not support ZEROPAGE_X addressing mode"
opSTX RELATIVE = error "Operation STX does not support RELATIVE addressing mode"
opSTX ABSOLUTE_X = error "Operation STX does not support ABSOLUTE_X addressing mode"
opSTX ABSOLUTE_Y = error "Operation STX does not support ABSOLUTE_Y addressing mode"
opSTX INDIRECT = error "Operation STX does not support INDIRECT addressing mode"
opSTX INDIRECT_X = error "Operation STX does not support INDIRECT_X addressing mode"
opSTX INDIRECT_Y = error "Operation STX does not support INDIRECT_Y addressing mode"
opSTX addr_mode = do
    addr <- getAddr addr_mode
    regx <- getIDX 
    writeByte addr regx

opSTY ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opSTY IMPLICIT = error "Operation STY does not support IMPLICIT addressing mode"
opSTY ACCUMULATOR = error "Operation STY does not support ACCUMULATOR addressing mode"
opSTY IMMEDIATE = error "Operation STY does not support IMMEDIATE addressing mode"
opSTY ZEROPAGE_Y = error "Operation STY does not support ZEROPAGE_X addressing mode"
opSTY RELATIVE = error "Operation STY does not support RELATIVE addressing mode"
opSTY ABSOLUTE_X = error "Operation STY does not support ABSOLUTE_X addressing mode"
opSTY ABSOLUTE_Y = error "Operation STY does not support ABSOLUTE_Y addressing mode"
opSTY INDIRECT = error "Operation STY does not support INDIRECT addressing mode"
opSTY INDIRECT_X = error "Operation STY does not support INDIRECT_X addressing mode"
opSTY INDIRECT_Y = error "Operation STY does not support INDIRECT_Y addressing mode"
opSTY addr_mode = do
    addr <- getAddr addr_mode
    regy <- getIDY 
    writeByte addr regy

opTAX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opTAX ACCUMULATOR = error "Operation TAX does not support ACCUMULATOR addressing mode"
opTAX IMMEDIATE = error "Operation TAX does not support IMMEDIATE addressing mode"
opTAX ZEROPAGE = error "Operation TAX does not support ZEROPAGE addressing mode"
opTAX ZEROPAGE_X = error "Operation TAX does not support ZEROPAGE_X addressing mode"
opTAX ZEROPAGE_Y = error "Operation TAX does not support ZEROPAGE_Y addressing mode"
opTAX RELATIVE = error "Operation TAX does not support RELATIVE addressing mode"
opTAX ABSOLUTE = error "Operation TAX does not support ABSOLUTE addressing mode"
opTAX ABSOLUTE_X = error "Operation TAX does not support ABSOLUTE_X addressing mode"
opTAX ABSOLUTE_Y = error "Operation TAX does not support ABSOLUTE_Y addressing mode"
opTAX INDIRECT = error "Operation TAX does not support INDIRECT addressing mode"
opTAX INDIRECT_X = error "Operation TAX does not support INDIRECT_X addressing mode"
opTAX INDIRECT_Y = error "Operation TAX does not support INDIRECT_Y addressing mode"
opTAX IMPLICIT = do
    acc <- getACC 
    setIDX acc
    setFlag ZERO (acc == 0)
    setFlag NEGATIVE (b7' acc)

opTAY ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opTAY ACCUMULATOR = error "Operation TAY does not support ACCUMULATOR addressing mode"
opTAY IMMEDIATE = error "Operation TAY does not support IMMEDIATE addressing mode"
opTAY ZEROPAGE = error "Operation TAY does not support ZEROPAGE addressing mode"
opTAY ZEROPAGE_X = error "Operation TAY does not support ZEROPAGE_X addressing mode"
opTAY ZEROPAGE_Y = error "Operation TAY does not support ZEROPAGE_Y addressing mode"
opTAY RELATIVE = error "Operation TAY does not support RELATIVE addressing mode"
opTAY ABSOLUTE = error "Operation TAY does not support ABSOLUTE addressing mode"
opTAY ABSOLUTE_X = error "Operation TAY does not support ABSOLUTE_X addressing mode"
opTAY ABSOLUTE_Y = error "Operation TAY does not support ABSOLUTE_Y addressing mode"
opTAY INDIRECT = error "Operation TAY does not support INDIRECT addressing mode"
opTAY INDIRECT_X = error "Operation TAY does not support INDIRECT_X addressing mode"
opTAY INDIRECT_Y = error "Operation TAY does not support INDIRECT_Y addressing mode"
opTAY IMPLICIT = do
    acc <- getACC 
    setIDY acc
    setFlag ZERO (acc == 0)
    setFlag NEGATIVE (b7' acc)

opTSX ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opTSX ACCUMULATOR = error "Operation TSX does not support ACCUMULATOR addressing mode"
opTSX IMMEDIATE = error "Operation TSX does not support IMMEDIATE addressing mode"
opTSX ZEROPAGE = error "Operation TSX does not support ZEROPAGE addressing mode"
opTSX ZEROPAGE_X = error "Operation TSX does not support ZEROPAGE_X addressing mode"
opTSX ZEROPAGE_Y = error "Operation TSX does not support ZEROPAGE_Y addressing mode"
opTSX RELATIVE = error "Operation TSX does not support RELATIVE addressing mode"
opTSX ABSOLUTE = error "Operation TSX does not support ABSOLUTE addressing mode"
opTSX ABSOLUTE_X = error "Operation TSX does not support ABSOLUTE_X addressing mode"
opTSX ABSOLUTE_Y = error "Operation TSX does not support ABSOLUTE_Y addressing mode"
opTSX INDIRECT = error "Operation TSX does not support INDIRECT addressing mode"
opTSX INDIRECT_X = error "Operation TSX does not support INDIRECT_X addressing mode"
opTSX INDIRECT_Y = error "Operation TSX does not support INDIRECT_Y addressing mode"
opTSX IMPLICIT = do
    sp <- getSP 
    setIDX sp
    setFlag ZERO (sp == 0)
    setFlag NEGATIVE (b7' sp)

opTXA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opTXA ACCUMULATOR = error "Operation TXA does not support ACCUMULATOR addressing mode"
opTXA IMMEDIATE = error "Operation TXA does not support IMMEDIATE addressing mode"
opTXA ZEROPAGE = error "Operation TXA does not support ZEROPAGE addressing mode"
opTXA ZEROPAGE_X = error "Operation TXA does not support ZEROPAGE_X addressing mode"
opTXA ZEROPAGE_Y = error "Operation TXA does not support ZEROPAGE_Y addressing mode"
opTXA RELATIVE = error "Operation TXA does not support RELATIVE addressing mode"
opTXA ABSOLUTE = error "Operation TXA does not support ABSOLUTE addressing mode"
opTXA ABSOLUTE_X = error "Operation TXA does not support ABSOLUTE_X addressing mode"
opTXA ABSOLUTE_Y = error "Operation TXA does not support ABSOLUTE_Y addressing mode"
opTXA INDIRECT = error "Operation TXA does not support INDIRECT addressing mode"
opTXA INDIRECT_X = error "Operation TXA does not support INDIRECT_X addressing mode"
opTXA INDIRECT_Y = error "Operation TXA does not support INDIRECT_Y addressing mode"
opTXA IMPLICIT = do
    xreg <- getIDX 
    setACC xreg
    setFlag ZERO (xreg == 0)
    setFlag NEGATIVE (b7' xreg)

opTXS ACCUMULATOR = error "Operation TXS does not support ACCUMULATOR addressing mode"
opTXS IMMEDIATE = error "Operation TXS does not support IMMEDIATE addressing mode"
opTXS ZEROPAGE = error "Operation TXS does not support ZEROPAGE addressing mode"
opTXS ZEROPAGE_X = error "Operation TXS does not support ZEROPAGE_X addressing mode"
opTXS ZEROPAGE_Y = error "Operation TXS does not support ZEROPAGE_Y addressing mode"
opTXS RELATIVE = error "Operation TXS does not support RELATIVE addressing mode"
opTXS ABSOLUTE = error "Operation TXS does not support ABSOLUTE addressing mode"
opTXS ABSOLUTE_X = error "Operation TXS does not support ABSOLUTE_X addressing mode"
opTXS ABSOLUTE_Y = error "Operation TXS does not support ABSOLUTE_Y addressing mode"
opTXS INDIRECT = error "Operation TXS does not support INDIRECT addressing mode"
opTXS INDIRECT_X = error "Operation TXS does not support INDIRECT_X addressing mode"
opTXS INDIRECT_Y = error "Operation TXS does not support INDIRECT_Y addressing mode"
opTXS IMPLICIT = do
    xreg <- getIDX 
    setSP xreg

opTYA ::ADDR_MODE -> StateT (MOS6502 a, a) IO ()
opTYA ACCUMULATOR = error "Operation TYA does not support ACCUMULATOR addressing mode"
opTYA IMMEDIATE = error "Operation TYA does not support IMMEDIATE addressing mode"
opTYA ZEROPAGE = error "Operation TYA does not support ZEROPAGE addressing mode"
opTYA ZEROPAGE_X = error "Operation TYA does not support ZEROPAGE_X addressing mode"
opTYA ZEROPAGE_Y = error "Operation TYA does not support ZEROPAGE_Y addressing mode"
opTYA RELATIVE = error "Operation TYA does not support RELATIVE addressing mode"
opTYA ABSOLUTE = error "Operation TYA does not support ABSOLUTE addressing mode"
opTYA ABSOLUTE_X = error "Operation TYA does not support ABSOLUTE_X addressing mode"
opTYA ABSOLUTE_Y = error "Operation TYA does not support ABSOLUTE_Y addressing mode"
opTYA INDIRECT = error "Operation TYA does not support INDIRECT addressing mode"
opTYA INDIRECT_X = error "Operation TYA does not support INDIRECT_X addressing mode"
opTYA INDIRECT_Y = error "Operation TYA does not support INDIRECT_Y addressing mode"
opTYA IMPLICIT = do
    yreg <- getIDY 
    setACC yreg
    setFlag ZERO (yreg == 0)
    setFlag NEGATIVE (b7' yreg)


--}
