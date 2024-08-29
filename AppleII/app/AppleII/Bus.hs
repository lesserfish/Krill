module AppleII.Bus where

import qualified AppleII.Memory as M
import qualified AppleII.Keyboard as KB
import qualified K6502

import Text.Printf
import Data.IORef
import Data.Word
import Control.Monad.State 

data AppleI = AppleI {
        applCPU       :: IORef K6502.K6502
    ,   applKeyboard  :: IORef KB.Keyboard
    ,   applRAM       :: M.Memory
    ,   applBIOS      :: M.Memory
}


cpuReadByte :: AppleI -> Word16 -> IO Word8
cpuReadByte apple address = undefined

cpuWriteByte :: AppleI -> Word16 -> Word8 -> IO ()
cpuWriteByte apple address byte = undefined

cpuWriteByte' :: AppleI -> Word16 -> Word8 -> IO ()
cpuWriteByte' apple address byte = do
    cpuWriteByte' apple address byte
    printf "CPU wrote byte %02X -> %04X\n" byte address

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

new :: [Word8] -> [Word8] -> IO AppleI
new bios cassette = do
    cpu <- newIORef K6502.new
    kb <- KB.new >>= newIORef
    ram <- M.fromList M.ReadAccess (replicate 0x1000 0 ++ cassette)
    rom <- M.fromList M.ReadOnly bios
    return $ AppleI {applCPU = cpu, applKeyboard = kb, applRAM = ram, applBIOS = rom}

sendKey :: AppleI -> Word8 -> IO ()
sendKey apple byte = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    kb' <- liftIO $ execStateT (KB.writeChar byte) kb
    liftIO $ writeIORef (applKeyboard apple) kb'

