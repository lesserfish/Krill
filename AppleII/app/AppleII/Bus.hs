module AppleII.Bus where

import qualified AppleII.Memory as M
import qualified AppleII.Keyboard as KB
import qualified AppleII.Display as D
import qualified K6502

import Data.IORef
import Data.Word
import Control.Monad.State 
import Control.Monad
import Foreign.Ptr

data AppleII = AppleII {
        applCPU       :: IORef K6502.K6502
    ,   applKeyboard  :: IORef KB.Keyboard
    ,   applRAM       :: M.Memory
    ,   applBIOS      :: M.Memory
    ,   applDisplay   :: IORef D.Display
}


cpuReadByte :: AppleII -> Word16 -> IO Word8
cpuReadByte _ address 
    | address <= 0xBFFF = return 0 -- RAM
    | address == 0xC000 = return 0 -- Keyboard Data
    | address == 0xC010 = return 0 -- Keyboard Strobe
    | address == 0xC020 = return 0 -- Cassette output plug
    | address == 0xC030 = return 0 -- Speaker
    | 0xC050 <= address && address <=  0xC057 = return 0 -- Display Switches
    | address == 0xC060 = return 0 -- Cassette input plug
    | otherwise = return 0

cpuWriteByte :: AppleII -> Word16 -> Word8 -> IO ()
cpuWriteByte _ address _
    | address <= 0xBFFF = return () -- RAM
    | address == 0xC000 = return () -- Keyboard Data
    | address == 0xC010 = return () -- Keyboard Strobe
    | address == 0xC020 = return () -- Cassette output plug
    | address == 0xC030 = return () -- Speaker
    | 0xC050 <= address && address <=  0xC057 = return () -- Display Switches
    | address == 0xC060 = return () -- Cassette input plug
    | otherwise = return ()

appleInterface :: AppleII -> K6502.Interface
appleInterface apple = K6502.Interface r w p where
    r = cpuReadByte apple
    w = cpuWriteByte apple
    p = r

tickDisplay :: AppleII -> IO ()
tickDisplay apple = do
    display <- readIORef (applDisplay apple)
    display' <- D.tick display
    writeIORef (applDisplay apple) display'


tick :: AppleII -> IO()
tick apple = do
    -- Tick CPU
    cpu <- liftIO . readIORef . applCPU $ apple
    cpu' <- K6502.tick (appleInterface apple) cpu
    writeIORef (applCPU apple) cpu'
    -- Tick Cassette
    -- Tick Display
    tickDisplay apple

tickN :: Int -> AppleII -> IO ()
tickN n apple = do
    replicateM_ n  (tick apple)

reset :: AppleII -> IO()
reset apple = do
    cpu <- liftIO . readIORef . applCPU $ apple
    cpu' <- K6502.reset (appleInterface apple) cpu
    writeIORef (applCPU apple) cpu'

new :: [Word8] -> [Word8] -> IO AppleII
new bios cassette = do
    cpu <- newIORef K6502.new
    kb <- KB.new >>= newIORef
    ram <- M.fromList M.ReadAccess (replicate 0x1000 0 ++ cassette)
    rom <- M.fromList M.ReadOnly bios
    display <- D.initialize ram >>= newIORef
    return $ AppleII {applCPU = cpu, applKeyboard = kb, applRAM = ram, applBIOS = rom, applDisplay = display}

updateVBuffer :: AppleII -> Ptr() -> IO()
updateVBuffer apple rawBuffer = do
    display <- readIORef (applDisplay apple)
    D.updateVBuffer display rawBuffer

sendKey :: AppleII -> Word8 -> IO ()
sendKey apple byte = do
    kb <- liftIO . readIORef . applKeyboard $ apple
    kb' <- liftIO $ execStateT (KB.writeChar byte) kb
    liftIO $ writeIORef (applKeyboard apple) kb'

