module K6502.Internal where

import K6502.Types
import Utils


import Control.Monad
import Control.Monad.State
import Data.Word
import Data.Bits

-- Getters and Setters

mapIP :: (Word16 -> Word16) -> StateT (K6502, Interface) IO ()
mapIP f = modifyFst (\k -> k{kRegisters = (kRegisters k){rIP = f . rIP . kRegisters $ k}})

mapSP :: (Word8 -> Word8) -> StateT (K6502, Interface) IO ()
mapSP f = modifyFst (\k -> k{kRegisters = (kRegisters k){rSP = f . rSP . kRegisters $ k}})

mapAX :: (Word8 -> Word8) -> StateT (K6502, Interface) IO ()
mapAX f = modifyFst (\k -> k{kRegisters = (kRegisters k){rAX = f . rAX . kRegisters $ k}})

mapIX :: (Word8 -> Word8) -> StateT (K6502, Interface) IO ()
mapIX f = modifyFst (\k -> k{kRegisters = (kRegisters k){rIX = f . rIX . kRegisters $ k}})

mapIY :: (Word8 -> Word8) -> StateT (K6502, Interface) IO ()
mapIY f = modifyFst (\k -> k{kRegisters = (kRegisters k){rIY = f . rIY . kRegisters $ k}})

mapFS :: (Word8 -> Word8) -> StateT (K6502, Interface) IO ()
mapFS f = modifyFst (\k -> k{kRegisters = (kRegisters k){rFS = f . rFS . kRegisters $ k}})



setIP :: Word16 -> StateT (K6502, Interface) IO ()
setIP v = mapIP (const v)

setSP :: Word8 -> StateT (K6502, Interface) IO ()
setSP v = mapSP (const v)

setAX :: Word8 -> StateT (K6502, Interface) IO ()
setAX v = mapAX (const v)

setIX :: Word8 -> StateT (K6502, Interface) IO ()
setIX v = mapIX (const v)

setIY :: Word8 -> StateT (K6502, Interface) IO ()
setIY v = mapIY (const v)

setFS :: Word8 -> StateT (K6502, Interface) IO ()
setFS v = mapFS (const v)



setIPIf :: Bool -> Word16 -> StateT (K6502, Interface) IO ()
setIPIf condition v = when condition $ setIP v 


setSPIf :: Bool -> Word8 -> StateT (K6502, Interface) IO ()
setSPIf condition v = when condition $ setSP v 


setAXIf :: Bool -> Word8 -> StateT (K6502, Interface) IO ()
setAXIf condition v = when condition $ setAX v


setIXIf :: Bool -> Word8 -> StateT (K6502, Interface) IO ()
setIXIf condition v = when condition $ setIX v


setIYIf :: Bool -> Word8 -> StateT (K6502, Interface) IO ()
setIYIf condition v = when condition $ setIY v 


setFSIf :: Bool -> Word8 -> StateT (K6502, Interface) IO ()
setFSIf condition v = when condition $ setFS v 



getIP :: StateT (K6502, Interface) IO Word16 
getIP = gets (rIP . kRegisters . fst)


getSP :: StateT (K6502, Interface) IO Word8 
getSP = gets (rSP . kRegisters . fst)


getAX :: StateT (K6502, Interface) IO Word8 
getAX = gets (rAX . kRegisters . fst)


getIX :: StateT (K6502, Interface) IO Word8 
getIX = gets (rIX . kRegisters .fst) 

getIY :: StateT (K6502, Interface) IO Word8 
getIY = gets (rIY . kRegisters . fst) 

getFS :: StateT (K6502, Interface) IO Word8 
getFS = gets (rFS . kRegisters . fst)


offsetIP :: Word16 -> StateT (K6502, Interface) IO Word16
offsetIP offset = do 
    v <- getIP
    setIP (v + offset)
    return v

offsetSP :: Word8 -> StateT (K6502, Interface) IO Word8
offsetSP offset = do 
    v <- getSP
    setSP (v + offset)
    return v

offsetAX :: Word8 -> StateT (K6502, Interface) IO Word8
offsetAX offset = do 
    v <- getAX
    setAX (v + offset)
    return v

offsetIY :: Word8 -> StateT (K6502, Interface) IO Word8
offsetIY offset = do 
    v <- getIY
    setIY (v + offset)
    return v

offsetCycles :: Int -> StateT (K6502, Interface) IO ()
offsetCycles offset = modifyFst (\k -> k{kCycles = offset + kCycles k})


getFlag :: FLAG -> StateT (K6502, Interface) IO Bool
getFlag CARRY             = b0 <$> getFS
getFlag ZERO              = b1 <$> getFS
getFlag INTERRUPT_DISABLE = b2 <$> getFS
getFlag DECIMAL_MODE      = b3 <$> getFS
getFlag BREAK_CMD         = b4 <$> getFS
getFlag OVERFLOW          = b6 <$> getFS
getFlag NEGATIVE          = b7 <$> getFS


setFlag :: FLAG -> Bool -> StateT (K6502, Interface) IO ()
setFlag CARRY flag             = mapFS (\reg -> if flag then setBit reg 0 else clearBit reg 0)
setFlag ZERO flag              = mapFS (\reg -> if flag then setBit reg 1 else clearBit reg 1)
setFlag INTERRUPT_DISABLE flag = mapFS (\reg -> if flag then setBit reg 2 else clearBit reg 2)
setFlag DECIMAL_MODE flag      = mapFS (\reg -> if flag then setBit reg 3 else clearBit reg 3)
setFlag BREAK_CMD flag         = mapFS (\reg -> if flag then setBit reg 4 else clearBit reg 4)
setFlag OVERFLOW flag          = mapFS (\reg -> if flag then setBit reg 6 else clearBit reg 6)
setFlag NEGATIVE flag          = mapFS (\reg -> if flag then setBit reg 7 else clearBit reg 7)


setFlagIf :: Bool -> FLAG -> Bool -> StateT (K6502, Interface) IO ()
setFlagIf condition flag value = when condition (setFlag flag value)

-- Context

setComplete :: Bool -> StateT (K6502, Interface) IO ()
setComplete b = modifyFst (\k -> k{kContext = (kContext k){ctxComplete = b}})

setDecimalEnabled:: Bool -> StateT (K6502, Interface) IO ()
setDecimalEnabled b = modifyFst (\k -> k{kContext = (kContext k){ctxDecimalEnabled = b}})

getDecimalEnabled  :: StateT (K6502, Interface) IO Bool
getDecimalEnabled = gets (ctxDecimalEnabled . kContext . fst)

setSuperInstruction :: Bool -> StateT (K6502, Interface) IO ()
setSuperInstruction v = modifyFst (\k -> k{kContext = (kContext k){ctxSuperInstruction = v}})

getSuperInstruction :: StateT (K6502, Interface) IO Bool
getSuperInstruction = gets (ctxSuperInstruction . kContext . fst)

getCycles :: StateT (K6502, Interface) IO Int
getCycles = gets $ kCycles . fst


getComplete :: StateT (K6502, Interface) IO Bool
getComplete = gets $ ctxComplete . kContext . fst


incClock :: StateT (K6502, Interface) IO ()
incClock = modifyFst (\k -> k{kClock = 1 + kClock k})


resetCycles :: StateT (K6502, Interface) IO ()
resetCycles = modifyFst (\k -> k{kCycles = 8})


resetClock :: StateT (K6502, Interface) IO ()
resetClock = modifyFst (\k -> k{kClock = 0})


fetchComplete :: StateT (K6502, Interface) IO Bool
fetchComplete = do
    c <- getComplete
    setComplete False
    return c


-- Read / Writes

readByte :: Word16 -> StateT (K6502, Interface) IO Word8
readByte address = do
    interface <- gets snd
    let reader = iReadByte interface
    liftIO $ reader address


writeByte :: Word16 -> Word8 -> StateT (K6502, Interface) IO ()
writeByte address byte = do
    interface <- gets snd
    let writer = iWriteByte interface
    liftIO $ writer address byte

writeStack :: Word8 -> StateT (K6502, Interface) IO ()
writeStack byte = do
    sp <- getSP
    let addr = 0x0100 + joinBytes 0x00 sp
    writeByte addr byte 
    mapSP (\x -> x - 1)


readStack :: StateT (K6502, Interface) IO Word8
readStack = do
    mapSP (+ 1)
    sp <- getSP
    let addr = 0x0100 + joinBytes 0x00 sp
    readByte addr


pageCrossSum :: Word16 -> Word16 -> StateT (K6502, Interface) IO Word16
pageCrossSum a b = do
    let output = a + b
    let pageCross = a .&. 0xFF00 /= output .&. 0xFF00
    super <- getSuperInstruction
    when (pageCross && super) (offsetCycles 1)
    return output


