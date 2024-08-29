module AppleI.Bus where

import qualified AppleI.Memory as M
import qualified AppleI.Keyboard as KB
import qualified AppleI.Terminal as T
import qualified K6502

import Text.Printf
import Data.IORef
import Data.Word
import Control.Monad.State 

data AppleI = AppleI {
        applCPU       :: IORef K6502.K6502
    ,   applKeyboard  :: IORef KB.Keyboard
    ,   applRAM       :: M.Memory
    ,   applTerminal  :: IORef T.Terminal
    ,   applBIOS      :: M.Memory
}


cpuReadByte :: AppleI -> Word16 -> IO Word8
cpuReadByte apple address
    | 0x0000 <= address && address <= 0x1FFF = readRAM 0x0000 apple address -- RAM mapped in 0x0000 : 0x0FFF
    | 0xD010 == address = readKBD apple
    | 0xD011 == address = readKBDCR apple
    | 0xD012 == address = readDSP apple
    | 0xD013 == address = readDSPCR apple
    | 0xE000 <= address && address <= 0xEFFF = readRAM 0xD000 apple address  -- RAM mapped in 0xE000 : 0xEFFF
    | 0xFF00 <= address && address <= 0xFFFF = readROM apple address
    | otherwise = return 0

cpuReadByte' :: AppleI -> Word16 -> IO Word8
cpuReadByte' apple address = do
    byte <- cpuReadByte' apple address
    printf "CPU read byte %02X <- %04X\n" byte address
    return byte


cpuWriteByte :: AppleI -> Word16 -> Word8 -> IO ()
cpuWriteByte apple address byte 
    | 0x0000 <= address && address <= 0x1FFF = writeRAM 0x0000 apple address byte
    | 0xD010 == address = writeKBD apple byte
    | 0xD011 == address = writeKBDCR apple byte
    | 0xD012 == address = writeDSP apple byte
    | 0xD013 == address = writeDSPCR apple byte
    | 0xE000 <= address && address <= 0xEFFF = writeRAM 0xD000 apple address byte
    | otherwise = return ()

cpuWriteByte' :: AppleI -> Word16 -> Word8 -> IO ()
cpuWriteByte' apple address byte = do
    cpuWriteByte' apple address byte
    printf "CPU wrote byte %02X -> %04X\n" byte address

readRAM :: Word16 -> AppleI -> Word16 -> IO Word8
readRAM offset apple address = do
    liftIO $ M.readByte (applRAM apple) (address - offset)

writeRAM :: Word16 -> AppleI -> Word16 -> Word8 -> IO ()
writeRAM offset apple address byte = do
    liftIO $ M.writeByte (applRAM apple) (address - offset) byte

readROM :: AppleI -> Word16 -> IO Word8
readROM apple address = do
    liftIO $ M.readByte (applBIOS apple) (address - 0xFF00)

readKBD :: AppleI -> IO Word8
readKBD apple = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    let char = KB.readChar kb
    kb' <- liftIO $ execStateT KB.clearStrobe kb

    liftIO $ writeIORef (applKeyboard apple) kb'
    return char

readKBDCR :: AppleI -> IO Word8
readKBDCR apple = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    return $ KB.readStrobe kb
    
writeKBD :: AppleI -> Word8 -> IO ()
writeKBD apple byte = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    kb' <- liftIO $ execStateT (KB.writeChar byte) kb
    liftIO $ writeIORef (applKeyboard apple) kb'


writeKBDCR :: AppleI -> Word8 -> IO ()
writeKBDCR apple _ = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    kb' <- liftIO $ execStateT KB.clearStrobe kb
    liftIO $ writeIORef (applKeyboard apple) kb'

readDSP :: AppleI -> IO Word8
readDSP apple = do
    term <- liftIO . readIORef . applTerminal $ apple
    return $ T.readChar term

readDSPCR :: AppleI -> IO Word8
readDSPCR _ = do
    return 0

writeDSP :: AppleI -> Word8 -> IO ()
writeDSP apple byte = do
    term <- liftIO . readIORef . applTerminal $ apple
    let term' = T.writeChar byte term
    liftIO $ writeIORef (applTerminal apple) term'

writeDSPCR :: AppleI -> Word8 -> IO ()
writeDSPCR apple _ = do
    term <- liftIO . readIORef . applTerminal $ apple
    term' <- execStateT T.reset term
    liftIO $ writeIORef (applTerminal apple) term'

appleInterface :: AppleI -> K6502.Interface
appleInterface apple = K6502.Interface r w p where
    r = cpuReadByte apple
    w = cpuWriteByte apple
    p = r

tick :: AppleI -> IO()
tick apple = do
    -- Tick the CPU
    cpu <- liftIO . readIORef . applCPU $ apple
    cpu' <- K6502.tick (appleInterface apple) cpu
    writeIORef (applCPU apple) cpu'
    -- Tick the terminal
    term <- liftIO . readIORef . applTerminal $ apple
    term' <- T.tick  term
    writeIORef (applTerminal apple) term'

tickN :: Int -> AppleI -> IO ()
tickN n apple = do
    let action = replicateM_ n (tick apple)
    action

reset :: AppleI -> IO()
reset apple = do
    -- Tick the CPU
    cpu <- liftIO . readIORef . applCPU $ apple
    cpu' <- K6502.reset (appleInterface apple) cpu
    writeIORef (applCPU apple) cpu'
    -- Tick the terminal
    term <- liftIO . readIORef . applTerminal $ apple
    term' <- execStateT T.reset term
    writeIORef (applTerminal apple) term'


new :: [Word8] -> [Word8] -> IO AppleI
new bios cassette = do
    cpu <- newIORef K6502.new
    term <- T.new >>= newIORef
    kb <- KB.new >>= newIORef
    ram <- M.fromList M.ReadAccess (replicate 0x1000 0 ++ cassette)
    rom <- M.fromList M.ReadOnly bios
    return $ AppleI {applCPU = cpu, applTerminal = term, applKeyboard = kb, applRAM = ram, applBIOS = rom}

sendKey :: AppleI -> Word8 -> IO ()
sendKey apple byte = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    kb' <- liftIO $ execStateT (KB.writeChar byte) kb
    liftIO $ writeIORef (applKeyboard apple) kb'


getVBuffer :: AppleI -> IO [Word8]
getVBuffer apple = do
    term <- liftIO . readIORef . applTerminal $ apple
    T.getVBuffer term

