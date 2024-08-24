module AppleI.Bus where

import Text.Printf
import qualified AppleI.Memory as M
import qualified AppleI.Keyboard as KB
import qualified AppleI.Terminal as T
import qualified K6502

import Data.IORef
import Data.Word
import Control.Monad.State 

data AppleI = AppleI {
        applCPU       :: IORef K6502.K6502
    ,   applKeyboard  :: IORef KB.Keyboard
    ,   applRAM       :: M.Memory
    ,   applTerminal  :: IORef T.Terminal
    ,   applROM       :: M.Memory
}


cpuReadByte' :: AppleI -> Word16 -> IO Word8
cpuReadByte' apple address
    | 0x0000 <= address && address <= 0x1FFF = readRAM apple address -- 8K bytes of ram (4K more than the supplied amount)
    | 0xD010 == address = readKBD apple
    | 0xD011 == address = readKBDCR apple
    | 0xD012 == address = readDSP apple
    | 0xD013 == address = readDSPCR apple
    | 0xFF00 <= address && address <= 0xFFFF = readROM apple address
    | otherwise = return 0xA1

cpuReadByte :: AppleI -> Word16 -> IO Word8
cpuReadByte apple address = do
    byte <- cpuReadByte' apple address
    when (address < 0xFF00 
            && address /= 0xD011 
            && address /= 0xD012
            && address /= 0xD010
            && address /= 0xD013
            ) (printf "Read byte %02X from address %04X \n" byte address)
    return byte


cpuWriteByte' :: AppleI -> Word16 -> Word8 -> IO ()
cpuWriteByte' apple address byte 
    | 0x0000 <= address && address <= 0x1FFF = writeRAM apple address byte
    | 0xD010 == address = writeKBD apple byte
    | 0xD011 == address = writeKBDCR apple byte
    | 0xD012 == address = writeDSP apple byte
    | 0xD013 == address = writeDSPCR apple byte
    | otherwise = return ()

cpuWriteByte :: AppleI -> Word16 ->  Word8 -> IO ()
cpuWriteByte apple address byte = do
    cpuWriteByte' apple address byte
    --when (address == 0xD012) (printf "Write byte %02X to display\n" byte)
    --printf "CPU write address %04X  <= %02X\n" address byte


readRAM :: AppleI -> Word16 -> IO Word8
readRAM apple address = do
    liftIO $ M.readByte (applRAM apple) address

writeRAM :: AppleI -> Word16 -> Word8 -> IO ()
writeRAM apple address byte = do
    liftIO $ M.writeByte (applRAM apple) address byte

readROM :: AppleI -> Word16 -> IO Word8
readROM apple address = do
    liftIO $ M.readByte (applROM apple) (address - 0xFF00)

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


printROM :: [Word8] -> IO ()
printROM [] = printf "\n"
printROM (y:ys) = do
    printf "%02X " y
    printROM ys

new :: [Word8] -> IO AppleI
new romData = do
    cpu <- newIORef K6502.new
    term <- T.new >>= newIORef
    kb <- KB.new >>= newIORef
    ram <- M.fromList M.ReadAccess (replicate 0x2000 0)
    rom <- M.fromList M.ReadOnly romData
    return $ AppleI {applCPU = cpu, applTerminal = term, applKeyboard = kb, applRAM = ram, applROM = rom}


sendKey :: AppleI -> Word8 -> IO ()
sendKey apple byte = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    kb' <- liftIO $ execStateT (KB.writeChar byte) kb
    liftIO $ writeIORef (applKeyboard apple) kb'


getVBuffer :: AppleI -> IO [Word8]
getVBuffer apple = do
    term <- liftIO . readIORef . applTerminal $ apple
    T.getVBuffer term


debug :: AppleI -> IO ()
debug apple = do
    cpu <- liftIO . readIORef . applCPU $ apple
    --print $ "CPU: " ++ (show . K6502.rIP . K6502.kRegisters $ cpu)
    return ()
