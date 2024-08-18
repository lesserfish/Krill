module AppleI.Bus where

import qualified AppleI.Memory as M
import qualified AppleI.Keyboard as KB
import qualified AppleI.Terminal as T
import qualified K6502

import Data.IORef
import Data.Word

data AppleI = AppleI {
        kbCPU       :: IORef K6502.K6502
    ,   kbKeyboard  :: IORef KB.Keyboard
    ,   kbRAM       :: M.Memory
    ,   kbTerminal  :: IORef T.Terminal
    ,   kbROM       :: M.Memory
}


cpuReadByte :: AppleI -> Word16 -> IO Word8
cpuReadByte kb address
    | 0x0000 <= address && address <= 0x1FFF = readRAM kb address
    | 0xD010 <= address && address <= 0xD011 = readKeyboard kb address
    | 0xD012 <= address && address <= 0xD013 = readTerminal kb address
    | 0x2000 <= address && address <= 0x23FF = readTerminal kb address
    | 0xD000 <= address && address <= 0xFFFF = readROM kb address
    | otherwise = return 0

cpuWriteByte :: AppleI -> Word16 -> Word8 -> IO ()
cpuWriteByte kb address byte 
    | 0x0000 <= address && address <= 0x1FFF = writeRAM kb address byte
    | 0x2000 <= address && address <= 0x23FF = writeTerminal kb address byte
    | 0x2400 <= address && address <= 0x240F = writeTerminal kb address byte
    | 0x3000 <= address && address <= 0x30FF = writeKeyboard kb address byte
    | 0xD000 <= address && address <= 0xFFFF = writeROM kb address byte
    | otherwise = return ()


readRAM :: AppleI -> Word16 -> IO Word8
readRAM kb address = do
    M.readByte (kbRAM kb) address

writeRAM :: AppleI -> Word16 -> Word8 -> IO ()
writeRAM kb address byte = do
    M.writeByte (kbRAM kb) address byte

readROM :: AppleI -> Word16 -> IO Word8
readROM kb address = do
    M.readByte (kbROM kb) (address - 0xD000)

writeROM :: AppleI -> Word16 -> Word8 -> IO ()
writeROM kb address byte = do
    M.writeByte (kbROM kb) (address - 0xD000) byte

readTerminal :: AppleI -> Word16 -> IO Word8
readTerminal kb address = do
    return 0
    -- display <- readIORef (kbTerminal kb)
    -- (byte, display') <- T.cpuReadByte address display
    -- writeIORef (kbTerminal kb) display'
    -- return byte

writeTerminal :: AppleI -> Word16 -> Word8 -> IO ()
writeTerminal kb address byte = do
    return ()
    -- display <- readIORef (kbTerminal kb)
    -- display' <- T.cpuWriteByte address byte display
    -- writeIORef (kbTerminal kb) display'


readKeyboard :: AppleI -> Word16 -> IO Word8
readKeyboard kb address = do
    keyboard <- readIORef (kbKeyboard kb)
    (byte, keyboard') <- KB.cpuReadByte address keyboard
    writeIORef (kbKeyboard kb) keyboard'
    return byte

writeKeyboard :: AppleI -> Word16 -> Word8 -> IO ()
writeKeyboard kb address byte = do
    keyboard <- readIORef (kbKeyboard kb)
    keyboard' <- KB.cpuWriteByte address byte keyboard
    writeIORef (kbKeyboard kb) keyboard'

-- new :: [Word8] -> IO AppleI
-- new romData = do
--     let cpu = K6502.new
--     keyboard <- KB.new
--     ram <- M.fromList M.ReadAccess (replicate 0x2000 0)
--     rom <- M.fromList M.ReadOnly romData
--     display <- D.new
--
--     cpur <- newIORef cpu
--     keyboardr <- newIORef keyboard
--     displayr <- newIORef display
--
--     return $ AppleI {kbCPU = cpur, kbKeyboard = keyboardr, kbTerminal = displayr, kbRAM = ram, kbROM = rom}

