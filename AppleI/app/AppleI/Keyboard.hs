module AppleI.Keyboard where

import Data.Word
import Control.Monad.State

data Context = Context

-- Address to check status
-- Address to check character
data Keyboard = Keyboard {
    kReady ::   Bool,
    kChar ::    Word8,
    kStatus ::  Word8,
    kContext :: Context
}

new :: IO Keyboard
new = do
    return $ Keyboard {kReady = True, kChar = 0, kStatus = 0, kContext = Context}

-- 0xD010 :: Read  | Character
-- 0xD011 :: Read  | Status
-- 0x3020 :: Write | Clear Strobe

readChar :: StateT Keyboard IO Word8
readChar = do
    gets kChar

readStatus :: StateT Keyboard IO Word8
readStatus = do
    gets kStatus

clearStrobe :: StateT Keyboard IO ()
clearStrobe = modify (\k -> k{kReady = True})

cpuReadByte' :: Word16 -> StateT Keyboard IO Word8
cpuReadByte' address
    | address == 0x3000 = readChar
    | address == 0x3010 = readStatus
    | otherwise = return 0

cpuWriteByte' :: Word16 -> Word8 -> StateT Keyboard IO ()
cpuWriteByte' address _
    | address == 0x3020 = clearStrobe
    | otherwise = return ()

cpuReadByte :: Word16 -> Keyboard -> IO (Word8, Keyboard)
cpuReadByte address = runStateT (cpuReadByte' address)

cpuWriteByte :: Word16 -> Word8 -> Keyboard -> IO Keyboard
cpuWriteByte address byte = execStateT (cpuWriteByte' address byte)
