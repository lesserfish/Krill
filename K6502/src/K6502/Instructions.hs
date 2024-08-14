module K6502.Instructions where

import K6502.Types
import K6502.Internal
import Utils

import Control.Monad.State
import Data.Word
import Data.Bits

reset ::StateT (K6502, Interface) IO () 
reset = do
    irq_lb <- readByte 0xFFFC 
    irq_hb <- readByte 0xFFFD 
    let jmp_addr = joinBytes irq_hb irq_lb
    setIP  jmp_addr 
    setSP  0xFD
    setAX 0x00
    setIX 0x00
    setIY 0x00
    setFS (setBit 0x00 5 :: Word8)
    resetClock
    resetCycles

-- Addressing modes
getAddr :: ADDR_MODE -> StateT (K6502, Interface) IO Word16
getAddr IMPLICIT    = return 0   -- Implicit does not require getAddr
getAddr ACCUMULATOR = return 0   -- Accumulator does not require getAddr
getAddr IMMEDIATE = do
    offsetIP 1 

getAddr ZEROPAGE = do
    operand <- offsetIP 1
    lb <- readByte operand
    return $ joinBytes 0x00 lb

getAddr ZEROPAGE_X = do
    operand <- offsetIP 1
    x <- getIX
    lb <- readByte operand
    return $ joinBytes 0x00 (lb + x)


getAddr ZEROPAGE_Y = do
    operand <- offsetIP 1
    y <- getIY
    lb <- readByte operand
    return $ joinBytes 0x00 (lb + y)

getAddr RELATIVE = do
    pc <- offsetIP 1
    offset <- asU16 <$> readByte pc
    let relativeOffset = if b7 offset then 0xFF00 .|. offset else offset
    address <- pageCrossSum pc relativeOffset
    return $ address + 1

getAddr ABSOLUTE = do
    operand <- offsetIP 2
    lb <- readByte operand 
    hb <- readByte (operand + 1)
    return $ joinBytes hb lb

getAddr ABSOLUTE_X = do
    operand <- offsetIP 2
    x <- getIX
    lb <- readByte operand
    hb <- readByte (operand + 1)
    pageCrossSum (joinBytes hb lb) (joinBytes 0x00 x)


getAddr ABSOLUTE_Y = do
    operand <- offsetIP 2
    y <- getIY
    lb <- readByte operand
    hb <- readByte (operand + 1)
    pageCrossSum (joinBytes hb lb) (joinBytes 0x00 y)


getAddr INDIRECT = do
    operand <- offsetIP 2
    let (pchb, pclb) = splitBytes operand

    addr_lb <- readByte (joinBytes pchb pclb)
    addr_hb <- readByte (joinBytes pchb pclb + 1)
    let addr1 = joinBytes addr_hb addr_lb
    lb <- readByte addr1 -- Get the actual address from that location
    let (hb1, lb1) = splitBytes addr1
    hb <- readByte (joinBytes hb1 (lb1 + 1))
    return $ joinBytes hb lb

getAddr INDIRECT_X = do
    operand <- offsetIP 1
    table_start <- readByte operand
    table_offset <- getIX
    let table_addr = table_start + table_offset 
    lb <- readByte (joinBytes 0x00 table_addr)
    hb <- readByte (joinBytes 0x00 (table_addr + 1))
    return $ joinBytes hb lb

getAddr INDIRECT_Y = do
    operand <- offsetIP 1
    table_lb <- readByte operand
    addr_lb <- readByte $ joinBytes 0x00 table_lb
    addr_hb <- readByte $ joinBytes 0x00 (table_lb + 1)
    let address = joinBytes addr_hb addr_lb
    y <- getIY
    pageCrossSum address (joinBytes 0x00 y)


-- Instruction Set

adcOverflow :: Bool -> Bool -> Bool -> Bool
adcOverflow x y r = xor r x && xor r y

opADC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opADC IMPLICIT = error "Operation ADC does not support IMPLICIT addressing mode"
opADC ACCUMULATOR = error "Operation ADC does not support ACCUMULATOR addressing mode"
opADC ZEROPAGE_Y = error "Operation ADC does not support ZEROPAGE_Y addressing mode"
opADC RELATIVE = error "Operation ADC does not support RELATIVE addressing mode"
opADC INDIRECT = error "Operation ADC does not support INDIRECT addressing mode"
opADC addr_mode = do
    decimalEnabled <- getDecimalEnabled
    decimalMode <- getFlag DECIMAL_MODE
    if decimalEnabled && decimalMode then opADCDec addr_mode else opADCReg addr_mode

-- Regular ADC
opADCReg :: ADDR_MODE -> StateT (K6502, Interface) IO ()
opADCReg addr_mode = do
    address <- getAddr addr_mode
    ax <- asU16 <$> getAX
    byte <- asU16 <$> readByte address
    carry_flag <- getFlag CARRY
    let carry = if carry_flag then 1 else 0
    let sum = ax + byte + carry
    setFlag CARRY (sum > 0xFF)
    setFlag ZERO (sum .&. 0xFF == 0)
    setFlag NEGATIVE (b7 sum)
    setFlag OVERFLOW (adcOverflow (b7 ax) (b7 byte) (b7 sum))
    setAX $ asU8 sum

-- Decimal mode ADC
opADCDec :: ADDR_MODE -> StateT (K6502, Interface) IO ()
opADCDec addr_mode = do
    address <- getAddr addr_mode
    ax <- asU16 <$> getAX
    byte <- asU16 <$> readByte address
    carry_flag <- getFlag CARRY
    let carry = if carry_flag then 1 else 0

    let a1 = ax .&. 0x0F
    let b1 = byte .&. 0x0F
    let c1 = carry
    let s1 = a1 + b1 + c1
    let o1 = s1 >= 0x0A

    let a2 = ax .&. 0xF0
    let b2 = byte .&. 0xF0
    let c2 = if o1 then 0x10 else 0x00
    let s2 = a2 + b2 + c2
    let o2 = s2 >= 0xA0

    let lsd = if o1 then (s1 + 0x06) .&. 0x0F else s1 .&. 0x0F
    let msd = if o2 then (s2 + 0x60) .&. 0xF0 else s2 .&. 0xF0

    let sum = msd + lsd

    setFlag CARRY o2
    setFlag ZERO ((ax + byte + carry) .&. 0xFF == 0)
    setFlag NEGATIVE (b7 s2)
    setFlag OVERFLOW (adcOverflow (b7 ax) (b7 byte) (b7 s2))
    setAX $ asU8 sum

opAND ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opAND IMPLICIT = error "Operation AND does not support IMPLICIT addressing mode"
opAND ACCUMULATOR = error "Operation AND does not support ACCUMULATOR addressing mode"
opAND ZEROPAGE_Y = error "Operation AND does not support ZEROPAGE_Y addressing mode"
opAND RELATIVE = error "Operation AND does not support RELATIVE addressing mode"
opAND INDIRECT = error "Operation AND does not support INDIRECT addressing mode"
opAND addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr 
    mapAX (.&. byte)
    ax <- getAX 
    setFlag ZERO (ax == 0)
    setFlag NEGATIVE (b7 ax)

opASL ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opASL IMPLICIT = error "Operation ASL does not support IMPLICIT addressing mode"
opASL IMMEDIATE = error "Operation ASL does not support IMMEDIATE addressing mode"
opASL ZEROPAGE_Y = error "Operation ASL does not support ZEROPAGE_Y addressing mode"
opASL RELATIVE = error "Operation ASL does not support RELATIVE addressing mode"
opASL ABSOLUTE_Y = error "Operation ASL does not support ABSOLUTE_Y addressing mode"
opASL INDIRECT = error "Operation ASL does not support INDIRECT addressing mode"
opASL INDIRECT_X = error "Operation ASL does not support INDIRECT_X addressing mode"
opASL INDIRECT_Y = error "Operation ASL does not support INDIRECT_Y addressing mode"
opASL ACCUMULATOR = do
    carry_flag <- b7 <$> getAX
    ax <- mapAX ( .<<. 1) >> getAX
    setFlag CARRY carry_flag 
    setFlag ZERO (ax == 0) 
    setFlag NEGATIVE (b7 ax) 
opASL addr_mode = do
    address <- getAddr addr_mode
    byte <- readByte address
    let carry_flag = b7 byte 
    let shifted_byte = byte .<<. 1
    writeByte address shifted_byte
    setFlag CARRY carry_flag 
    setFlag ZERO (shifted_byte == 0) 
    setFlag NEGATIVE (b7 shifted_byte)

opBCC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    carry_flag <- getFlag CARRY 
    unless carry_flag (offsetCycles 1)
    unless carry_flag (setSuperInstruction True)
    address <- getAddr RELATIVE
    unless carry_flag (setIP address)

opBCS ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    carry_flag <- getFlag CARRY 
    when carry_flag (offsetCycles 1)
    when carry_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE 
    when carry_flag (setIP addr)

opBEQ ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    zero_flag <- getFlag ZERO 
    when zero_flag (offsetCycles 1)
    when zero_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE
    when zero_flag (setIP addr)

opBIT ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    address <- getAddr addr_mode 
    byte <- readByte address
    ax <- getAX 
    let and_result = byte .&. ax 
    setFlag ZERO (and_result == 0)
    setFlag NEGATIVE (b7 byte)
    setFlag OVERFLOW (b6 byte)

opBMI ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    negative_flag <- getFlag NEGATIVE
    when negative_flag (offsetCycles 1)
    when negative_flag (setSuperInstruction True)
    address <- getAddr RELATIVE
    when negative_flag (setIP address)

opBNE ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    zero_flag <- getFlag ZERO
    unless zero_flag (offsetCycles 1)
    unless zero_flag (setSuperInstruction True)
    address <- getAddr RELATIVE 
    unless zero_flag (setIP address)

opBPL ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    negative_flag <- getFlag NEGATIVE 
    unless negative_flag (offsetCycles 1)
    unless negative_flag (setSuperInstruction True)
    address <- getAddr RELATIVE 
    unless negative_flag (setIP address)

opBRK ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    pushed_pc <- (+ 1) <$> getIP
    let (pchb, pclb) = splitBytes pushed_pc 
    ps <- getFS 
    writeStack pchb
    writeStack pclb
    writeStack (setBit ps 4)
    irq_lb <- readByte 0xFFFE 
    irq_hb <- readByte 0xFFFF
    let jmp_addr = joinBytes irq_hb irq_lb
    setIP jmp_addr
    setFlag INTERRUPT_DISABLE True 


opBVC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    overflow_flag <- getFlag OVERFLOW 
    unless overflow_flag (offsetCycles 1)
    unless overflow_flag (setSuperInstruction True)
    address <- getAddr RELATIVE 
    unless overflow_flag (setIP address)

opBVS ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    overflow_flag <- getFlag OVERFLOW 
    when overflow_flag (offsetCycles 1)
    when overflow_flag (setSuperInstruction True)
    addr <- getAddr RELATIVE 
    when overflow_flag (setIP addr)

opCLC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opCLD ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opCLI ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opCLV ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opCMP ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opCMP IMPLICIT = error "Operation CMP does not support IMPLICIT addressing mode"
opCMP ACCUMULATOR = error "Operation CMP does not support ACCUMULATOR addressing mode"
opCMP ZEROPAGE_Y = error "Operation CMP does not support ZEROPAGE_Y addressing mode"
opCMP RELATIVE = error "Operation CMP does not support RELATIVE addressing mode"
opCMP INDIRECT = error "Operation CMP does not support INDIRECT addressing mode"
opCMP addr_mode = do
    ax <- getAX 
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    let result = ax - byte 
    setFlag ZERO (ax == byte) 
    setFlag CARRY (ax >= byte) 
    setFlag NEGATIVE (b7 result) 

opCPX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    xreg <- getIX 
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    let result = xreg - byte 
    setFlag ZERO (xreg == byte) 
    setFlag CARRY (xreg >= byte) 
    setFlag NEGATIVE (b7 result) 

opCPY ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    yreg <- getIY 
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    let result = yreg - byte 
    setFlag ZERO (yreg == byte) 
    setFlag CARRY (yreg >= byte) 
    setFlag NEGATIVE (b7 result) 

opDEC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    let result = byte - 1
    writeByte addr result
    setFlag ZERO (result == 0) 
    setFlag NEGATIVE (b7 result) 

opDEX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    mapIX (\x -> x - (1 :: Word8)) 
    idx <- getIX 
    setFlag ZERO (idx == 0) 
    setFlag NEGATIVE (b7 idx) 

opDEY ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    mapIY (\x -> x - (1 :: Word8)) 
    idy <- getIY 
    setFlag ZERO (idy == 0) 
    setFlag NEGATIVE (b7 idy) 

opEOR ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opEOR IMPLICIT = error "Operation EOR does not support IMPLICIT addressing mode"
opEOR ACCUMULATOR = error "Operation EOR does not support ACCUMULATOR addressing mode"
opEOR ZEROPAGE_Y = error "Operation EOR does not support ZEROPAGE_Y addressing mode"
opEOR RELATIVE = error "Operation EOR does not support RELATIVE addressing mode"
opEOR INDIRECT = error "Operation EOR does not support INDIRECT addressing mode"
opEOR addr_mode = do
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    mapAX (`xor` byte) 
    ax <- getAX 
    setFlag ZERO (ax == 0) 
    setFlag NEGATIVE (b7 ax) 

opINC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    let result = byte + 1
    writeByte addr result
    setFlag ZERO (result == 0) 
    setFlag NEGATIVE (b7 result) 

opINX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    mapIX (+ (1 :: Word8)) 
    idx <- getIX 
    setFlag ZERO (idx == 0) 
    setFlag NEGATIVE (b7 idx) 

opINY ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    mapIY (+ (1 :: Word8)) 
    idy <- getIY 
    setFlag ZERO (idy == 0) 
    setFlag NEGATIVE (b7 idy) 

opJMP ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    addr <- getAddr addr_mode 
    setIP addr

opJSR ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    
    
    
    pc <- getIP 
    let (hb, lb) = splitBytes (pc + 1)
    writeStack hb
    addr <- getAddr addr_mode 
    writeStack lb
    setIP addr

opLDA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opLDA IMPLICIT = error "Operation LDA does not support IMPLICIT addressing mode"
opLDA ACCUMULATOR = error "Operation LDA does not support ACCUMULATOR addressing mode"
opLDA ZEROPAGE_Y = error "Operation LDA does not support ZEROPAGE_Y addressing mode"
opLDA RELATIVE = error "Operation LDA does not support RELATIVE addressing mode"
opLDA INDIRECT = error "Operation LDA does not support INDIRECT addressing mode"
opLDA addr_mode = do
    addr <- getAddr addr_mode 
    byte <- readByte addr
    setAX byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7 byte)

opLDX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opLDX IMPLICIT = error "Operation LDX does not support IMPLICIT addressing mode"
opLDX ACCUMULATOR = error "Operation LDX does not support ACCUMULATOR addressing mode"
opLDX ZEROPAGE_X = error "Operation LDX does not support ZEROPAGE_X addressing mode"
opLDX RELATIVE = error "Operation LDX does not support RELATIVE addressing mode"
opLDX ABSOLUTE_X = error "Operation LDX does not support ABSOLUTE_X addressing mode"
opLDX INDIRECT = error "Operation LDX does not support INDIRECT addressing mode"
opLDX INDIRECT_X = error "Operation LDX does not support INDIRECT_X addressing mode"
opLDX INDIRECT_Y = error "Operation LDX does not support INDIRECT_Y addressing mode"
opLDX addr_mode = do
    addr <- getAddr addr_mode 
    byte <- readByte addr
    setIX byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7 byte)

opLDY ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opLDY IMPLICIT = error "Operation LDY does not support IMPLICIT addressing mode"
opLDY ACCUMULATOR = error "Operation LDY does not support ACCUMULATOR addressing mode"
opLDY ZEROPAGE_Y = error "Operation LDY does not support ZEROPAGE_X addressing mode"
opLDY RELATIVE = error "Operation LDY does not support RELATIVE addressing mode"
opLDY ABSOLUTE_Y = error "Operation LDY does not support ABSOLUTE_X addressing mode"
opLDY INDIRECT = error "Operation LDY does not support INDIRECT addressing mode"
opLDY INDIRECT_X = error "Operation LDY does not support INDIRECT_X addressing mode"
opLDY INDIRECT_Y = error "Operation LDY does not support INDIRECT_Y addressing mode"
opLDY addr_mode = do
    addr <- getAddr addr_mode 
    byte <- readByte addr
    setIY byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7 byte)

opLSR ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opLSR IMPLICIT = error "Operation LSR does not support IMPLICIT addressing mode"
opLSR IMMEDIATE = error "Operation LSR does not support IMMEDIATE addressing mode"
opLSR ZEROPAGE_Y = error "Operation LSR does not support ZEROPAGE_Y addressing mode"
opLSR RELATIVE = error "Operation LSR does not support RELATIVE addressing mode"
opLSR ABSOLUTE_Y = error "Operation LSR does not support ABSOLUTE_Y addressing mode"
opLSR INDIRECT = error "Operation LSR does not support INDIRECT addressing mode"
opLSR INDIRECT_X = error "Operation LSR does not support INDIRECT_X addressing mode"
opLSR INDIRECT_Y = error "Operation LSR does not support INDIRECT_Y addressing mode"
opLSR ACCUMULATOR = do
    carry_flag <- b0 <$> getAX
    mapAX ( .>>. 1)
    ax <- getAX 
    setFlag CARRY carry_flag
    setFlag ZERO (ax == 0) 
    setFlag NEGATIVE (b7 ax) 
opLSR addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    let carry_flag = b0 byte 
    let new_byte = byte .>>. 1 
    writeByte addr new_byte
    setFlag CARRY carry_flag
    setFlag ZERO (new_byte == 0) 
    setFlag NEGATIVE (b7 new_byte) 

opNOP ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opORA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opORA IMPLICIT = error "Operation ORA does not support IMPLICIT addressing mode"
opORA ACCUMULATOR = error "Operation ORA does not support ACCUMULATOR addressing mode"
opORA ZEROPAGE_Y = error "Operation ORA does not support ZEROPAGE_Y addressing mode"
opORA RELATIVE = error "Operation ORA does not support RELATIVE addressing mode"
opORA INDIRECT = error "Operation ORA does not support INDIRECT addressing mode"
opORA addr_mode = do
    addr <- getAddr addr_mode 
    byte <- readByte addr 
    mapAX (.|. byte) 
    ax <- getAX 
    setFlag ZERO (ax == 0) 
    setFlag NEGATIVE (b7 ax)

opPHA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    ax <- getAX 
    writeStack ax

opPHP ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    ps <- getFS 
    writeStack (setBit ps 4)

opPLA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    ax <- readStack
    setAX ax
    setFlag ZERO (ax == 0)
    setFlag NEGATIVE (b7 ax)

opPLP ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    setFS ps''

opROL ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opROL IMPLICIT = error "Operation ROL does not support IMPLICIT addressing mode"
opROL IMMEDIATE = error "Operation ROL does not support IMMEDIATE addressing mode"
opROL ZEROPAGE_Y = error "Operation ROL does not support ZEROPAGE_Y addressing mode"
opROL RELATIVE = error "Operation ROL does not support RELATIVE addressing mode"
opROL ABSOLUTE_Y = error "Operation ROL does not support ABSOLUTE_Y addressing mode"
opROL INDIRECT = error "Operation ROL does not support INDIRECT addressing mode"
opROL INDIRECT_X = error "Operation ROL does not support INDIRECT_X addressing mode"
opROL INDIRECT_Y = error "Operation ROL does not support INDIRECT_Y addressing mode"
opROL ACCUMULATOR = do
    ax <- getAX 
    carry_flag <- getFlag CARRY
    let new_carry = b7 ax
    let bit0 = if carry_flag then (0x01 :: Word8) else (0x00 :: Word8)
    let ax' = (ax .<<. 1) .|. bit0
    setAX ax'
    setFlag ZERO (ax' == 0)
    setFlag NEGATIVE (b7 ax')
    setFlag CARRY new_carry
opROL addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    carry_flag <- getFlag CARRY
    let new_carry = b7 byte
    let bit0 = if carry_flag then (0x01 :: Word8) else (0x00 :: Word8)
    let byte' = (byte .<<. 1) .|. bit0
    writeByte addr byte'
    setFlag ZERO (byte' == 0)
    setFlag NEGATIVE (b7 byte')
    setFlag CARRY new_carry

opROR ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opROR IMPLICIT = error "Operation ROR does not support IMPLICIT addressing mode"
opROR IMMEDIATE = error "Operation ROR does not support IMMEDIATE addressing mode"
opROR ZEROPAGE_Y = error "Operation ROR does not support ZEROPAGE_Y addressing mode"
opROR RELATIVE = error "Operation ROR does not support RELATIVE addressing mode"
opROR ABSOLUTE_Y = error "Operation ROR does not support ABSOLUTE_Y addressing mode"
opROR INDIRECT = error "Operation ROR does not support INDIRECT addressing mode"
opROR INDIRECT_X = error "Operation ROR does not support INDIRECT_X addressing mode"
opROR INDIRECT_Y = error "Operation ROR does not support INDIRECT_Y addressing mode"
opROR ACCUMULATOR = do
    ax <- getAX 
    carry_flag <- getFlag CARRY
    let new_carry = b0 ax
    let bit0 = if carry_flag then (0x80 :: Word8) else (0x00 :: Word8)
    let ax' = (ax .>>. 1) .|. bit0
    setAX ax'
    setFlag ZERO (ax' == 0)
    setFlag NEGATIVE (b7 ax')
    setFlag CARRY new_carry
opROR addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    carry_flag <- getFlag CARRY
    let new_carry = b0 byte
    let bit0 = if carry_flag then (0x80 :: Word8) else (0x00 :: Word8)
    let byte' = (byte .>>. 1) .|. bit0
    writeByte addr byte'
    setFlag ZERO (byte' == 0)
    setFlag NEGATIVE (b7 byte')
    setFlag CARRY new_carry

opRTI ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    setFS ps''
    setIP pc

opRTS ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    let pc = joinBytes pchb pclb + 1
    setIP pc

opSBC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opSBC IMPLICIT = error "Operation SBC does not support IMPLICIT addressing mode"
opSBC ACCUMULATOR = error "Operation SBC does not support ACCUMULATOR addressing mode"
opSBC ZEROPAGE_Y = error "Operation SBC does not support ZEROPAGE_Y addressing mode"
opSBC RELATIVE = error "Operation SBC does not support RELATIVE addressing mode"
opSBC INDIRECT = error "Operation SBC does not support INDIRECT addressing mode"
opSBC addr_mode = do
    decimalEnabled <- getDecimalEnabled
    decimalMode <- getFlag DECIMAL_MODE
    if decimalEnabled && decimalMode then opSBCDec addr_mode else opSBCReg addr_mode

sbcOverflow :: Bool -> Bool -> Bool -> Bool
sbcOverflow x y r = (xor x r) && not (xor y r)
-- Regular ADC
opSBCReg :: ADDR_MODE -> StateT (K6502, Interface) IO ()
opSBCReg addr_mode = do
    address <- getAddr addr_mode
    ax <- asU16 <$> getAX
    byte' <- asU16 <$> readByte address
    let byte = xor byte 0x00FF
    carry_flag <- getFlag CARRY
    let carry = if carry_flag then 1 else 0
    let sum = ax + byte + carry
    setFlag CARRY (sum > 0xFF)
    setFlag ZERO (sum .&. 0xFF == 0)
    setFlag NEGATIVE (b7 sum)
    setFlag OVERFLOW (sbcOverflow (b7 ax) (b7 byte) (b7 sum))
    setAX $ asU8 sum

-- Decimal mode ADC
opSBCDec :: ADDR_MODE -> StateT (K6502, Interface) IO ()
opSBCDec addr_mode = do
    address <- getAddr addr_mode
    ax <- asU16 <$> getAX
    byte <- asU16 <$> readByte address
    carry_flag <- getFlag CARRY
    let carry = if carry_flag then 1 else 0

    let a1 = ax .&. 0x0F
    let b1 = byte .&. 0x0F
    let c1 = xor carry 1
    let s1 = a1 - b1 - c1
    let o1 = s1 > 0x0F

    let a2 = ax .&. 0xF0
    let b2 = byte .&. 0xF0
    let c2 = if o1 then 0xFFF0 else 0x00
    let s2 = a2 - b2 - c2
    let o2 = s2 > 0xFF

    let lsd = if o1 then (s1 - 0x06) .&. 0x0F else s1 .&. 0x0F
    let msd = if o2 then (s2 - 0x60) .&. 0xF0 else s2 .&. 0xF0

    let sum = msd + lsd

    setFlag CARRY (not o2)
    setFlag ZERO ((ax + xor byte 0xFF + carry) .&. 0xFF == 0)
    setFlag NEGATIVE (b7 s2)
    setFlag OVERFLOW (sbcOverflow (b7 ax) (b7 byte) (b7 s2))
    setAX $ asU8 sum



opSEC ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opSED ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opSEI ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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

opSTA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
opSTA IMPLICIT = error "Operation STA does not support IMPLICIT addressing mode"
opSTA ACCUMULATOR = error "Operation STA does not support ACCUMULATOR addressing mode"
opSTA IMMEDIATE = error "Operation STA does not support IMMEDIATE addressing mode"
opSTA ZEROPAGE_Y = error "Operation STA does not support ZEROPAGE_Y addressing mode"
opSTA RELATIVE = error "Operation STA does not support RELATIVE addressing mode"
opSTA INDIRECT = error "Operation STA does not support INDIRECT addressing mode"
opSTA addr_mode = do
    addr <- getAddr addr_mode
    ax <- getAX 
    writeByte addr ax

opSTX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    regx <- getIX 
    writeByte addr regx

opSTY ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    regy <- getIY 
    writeByte addr regy

opTAX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    ax <- getAX 
    setIX ax
    setFlag ZERO (ax == 0)
    setFlag NEGATIVE (b7 ax)

opTAY ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    ax <- getAX 
    setIY ax
    setFlag ZERO (ax == 0)
    setFlag NEGATIVE (b7 ax)

opTSX ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    setIX sp
    setFlag ZERO (sp == 0)
    setFlag NEGATIVE (b7 sp)

opTXA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    xreg <- getIX 
    setAX xreg
    setFlag ZERO (xreg == 0)
    setFlag NEGATIVE (b7 xreg)

opTXS ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    xreg <- getIX 
    setSP xreg

opTYA ::ADDR_MODE -> StateT (K6502, Interface) IO ()
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
    yreg <- getIY 
    setAX yreg
    setFlag ZERO (yreg == 0)
    setFlag NEGATIVE (b7 yreg)
