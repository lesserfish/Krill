module K6502.Internal where

import K6502.Types
import Utils


import Control.Monad(when)
import Control.Monad.State
import Data.Word
import Data.Bits
import K6502.Types (K6502(kInterface))

-- Getters and Setters

mapIP :: (Word16 -> Word16) -> StateT K6502 IO ()
mapIP f = modify (\k -> k{kRegisters = (kRegisters k){ip = f . ip . kRegisters $ k}})

mapSP :: (Word8 -> Word8) -> StateT K6502 IO ()
mapSP f = modify (\k -> k{kRegisters = (kRegisters k){sp = f . sp . kRegisters $ k}})

mapAX :: (Word8 -> Word8) -> StateT K6502 IO ()
mapAX f = modify (\k -> k{kRegisters = (kRegisters k){ax = f . ax . kRegisters $ k}})

mapIX :: (Word8 -> Word8) -> StateT K6502 IO ()
mapIX f = modify (\k -> k{kRegisters = (kRegisters k){ix = f . ix . kRegisters $ k}})

mapIY :: (Word8 -> Word8) -> StateT K6502 IO ()
mapIY f = modify (\k -> k{kRegisters = (kRegisters k){iy = f . iy . kRegisters $ k}})

mapFS :: (Word8 -> Word8) -> StateT K6502 IO ()
mapFS f = modify (\k -> k{kRegisters = (kRegisters k){fs = f . fs . kRegisters $ k}})



setIP :: Word16 -> StateT K6502 IO ()
setIP v = mapIP (const v)

setSP :: Word8 -> StateT K6502 IO ()
setSP v = mapSP (const v)

setAX :: Word8 -> StateT K6502 IO ()
setAX v = mapAX (const v)

setIX :: Word8 -> StateT K6502 IO ()
setIX v = mapIX (const v)

setIY :: Word8 -> StateT K6502 IO ()
setIY v = mapIY (const v)

setFS :: Word8 -> StateT K6502 IO ()
setFS v = mapFS (const v)



setIPIf :: Bool -> Word16 -> StateT K6502 IO ()
setIPIf condition v = when condition $ setIP v 


setSPIf :: Bool -> Word8 -> StateT K6502 IO ()
setSPIf condition v = when condition $ setSP v 


setAXIf :: Bool -> Word8 -> StateT K6502 IO ()
setAXIf condition v = when condition $ setAX v


setIXIf :: Bool -> Word8 -> StateT K6502 IO ()
setIXIf condition v = when condition $ setIX v


setIYIf :: Bool -> Word8 -> StateT K6502 IO ()
setIYIf condition v = when condition $ setIY v 


setFSIf :: Bool -> Word8 -> StateT K6502 IO ()
setFSIf condition v = when condition $ setFS v 



getIP :: StateT K6502 IO Word16 
getIP = gets (ip . kRegisters)


getSP :: StateT K6502 IO Word8 
getSP = gets (sp . kRegisters)


getAX :: StateT K6502 IO Word8 
getAX = gets (ax . kRegisters)


getIX :: StateT K6502 IO Word8 
getIX = gets (ix . kRegisters) 

getIY :: StateT K6502 IO Word8 
getIY = gets (iy . kRegisters) 

getFS :: StateT K6502 IO Word8 
getFS = gets (fs . kRegisters)


offsetIP :: Word16 -> StateT K6502 IO Word16
offsetIP offset = do 
    v <- getIP
    setIP (v + offset)
    return v

offsetSP :: Word8 -> StateT K6502 IO Word8
offsetSP offset = do 
    v <- getSP
    setSP (v + offset)
    return v

offsetAX :: Word8 -> StateT K6502 IO Word8
offsetAX offset = do 
    v <- getAX
    setAX (v + offset)
    return v

offsetIY :: Word8 -> StateT K6502 IO Word8
offsetIY offset = do 
    v <- getIY
    setIY (v + offset)
    return v

offsetCycles :: Int -> StateT K6502 IO ()
offsetCycles offset = modify (\k -> k{kCycles = offset + kCycles k})


getFlag :: FLAG -> StateT K6502 IO Bool
getFlag CARRY             = b0 <$> getFS
getFlag ZERO              = b1 <$> getFS
getFlag INTERRUPT_DISABLE = b2 <$> getFS
getFlag DECIMAL_MODE      = b3 <$> getFS
getFlag BREAK_CMD         = b4 <$> getFS
getFlag OVERFLOW          = b6 <$> getFS
getFlag NEGATIVE          = b7 <$> getFS


setFlag :: FLAG -> Bool -> StateT K6502 IO ()
setFlag CARRY flag             = mapFS (\reg -> if flag then setBit reg 0 else clearBit reg 0)
setFlag ZERO flag              = mapFS (\reg -> if flag then setBit reg 1 else clearBit reg 1)
setFlag INTERRUPT_DISABLE flag = mapFS (\reg -> if flag then setBit reg 2 else clearBit reg 2)
setFlag DECIMAL_MODE flag      = mapFS (\reg -> if flag then setBit reg 3 else clearBit reg 3)
setFlag BREAK_CMD flag         = mapFS (\reg -> if flag then setBit reg 4 else clearBit reg 4)
setFlag OVERFLOW flag          = mapFS (\reg -> if flag then setBit reg 6 else clearBit reg 6)
setFlag NEGATIVE flag          = mapFS (\reg -> if flag then setBit reg 7 else clearBit reg 7)


setFlagIf :: Bool -> FLAG -> Bool -> StateT K6502 IO ()
setFlagIf condition flag value = when condition (setFlag flag value)

-- Context

setComplete :: Bool -> StateT K6502 IO ()
setComplete b = modify (\k -> k{kContext = (kContext k){ctxComplete = b}})

setDecimalEnabled:: Bool -> StateT K6502 IO ()
setDecimalEnabled b = modify (\k -> k{kContext = (kContext k){ctxDecimalEnabled = b}})

getDecimalEnabled  :: StateT K6502 IO Bool
getDecimalEnabled = gets (ctxDecimalEnabled . kContext)

setSuperInstruction :: Bool -> StateT K6502 IO ()
setSuperInstruction v = modify (\k -> k{kContext = (kContext k){ctxSuperInstruction = v}})

getSuperInstruction :: StateT K6502 IO Bool
getSuperInstruction = gets (ctxSuperInstruction . kContext)

getCycles :: StateT K6502 IO Int
getCycles = gets kCycles


getComplete :: StateT K6502 IO Bool
getComplete = gets $ ctxComplete . kContext


incClock :: StateT K6502 IO ()
incClock = modify (\k -> k{kClock = 1 + kClock k})


resetCycles :: StateT K6502 IO ()
resetCycles = modify (\k -> k{kCycles = 8})


resetClock :: StateT K6502 IO ()
resetClock = modify (\k -> k{kClock = 0})


fetchComplete :: StateT K6502 IO Bool
fetchComplete = do
    c <- getComplete
    setComplete False
    return c


-- Read / Writes

readByte :: Word16 -> StateT K6502 IO Word8
readByte address = do
    k <- get
    let reader = iReadByte . kInterface $ k
    liftIO $ reader address


writeByte :: Word16 -> Word8 -> StateT K6502 IO ()
writeByte address byte = do
    k <- get
    let writer = iWriteByte . kInterface $ k
    liftIO $ writer address byte

writeStack :: Word8 -> StateT K6502 IO ()
writeStack byte = do
    sp <- getSP
    let addr = 0x0100 + joinBytes 0x00 sp
    writeByte addr byte 
    mapSP (\x -> x - 1)


readStack :: StateT K6502 IO Word8
readStack = do
    mapSP (+ 1)
    sp <- getSP
    let addr = 0x0100 + joinBytes 0x00 sp
    readByte addr


-- Addresing Modes

-- Helpers

pageCross :: Word16 -> Word16 -> StateT K6502 IO ()
pageCross x y = do
    let z = x + y
    let check = (z .&. 0xFF00) == (x .&. 0xFF)
    super <- getSuperInstruction
    when (check && super) (offsetCycles 1)


getAddr :: ADDR_MODE -> StateT K6502 IO Word16
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
    operand <- offsetIP 1
    offset <- readByte operand 
    
    pc <- getIP
    return $ pc + toU16 offset

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
    let address = joinBytes hb lb + joinBytes 0x00 x
    pageCross (joinBytes hb lb) (joinBytes 0x00 x)
    return address


getAddr ABSOLUTE_Y = do
    operand <- offsetIP 2
    y <- getIY
    lb <- readByte operand
    hb <- readByte (operand + 1)
    let address = joinBytes hb lb + joinBytes 0x00 y
    pageCross (joinBytes hb lb) (joinBytes 0x00 y)
    return address


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
    let fullAddress = address + joinBytes 0x00 y
    pageCross address (joinBytes 0x00 y)
    return fullAddress
