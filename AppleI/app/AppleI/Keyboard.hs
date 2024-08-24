module AppleI.Keyboard where

import Data.Word
import Control.Monad.State

data Context = Context

-- Address to check status
-- Address to check character
data Keyboard = Keyboard {
    kChar ::    Word8,
    kStrobe ::  Word8,
    kContext :: Context
}

new :: IO Keyboard
new = do
    return $ Keyboard {kChar = 0, kStrobe = 0, kContext = Context}

-- 0xD010 :: Read  | Character
-- 0xD011 :: Read  | Status
-- 0x3020 :: Write | Clear Strobe

readChar :: Keyboard -> Word8
readChar = kChar

readStrobe :: Keyboard -> Word8
readStrobe = kStrobe

clearStrobe :: StateT Keyboard IO ()
clearStrobe = modify (\k -> k{kStrobe = 0})

writeChar :: Word8 -> StateT Keyboard IO ()
writeChar char = modify (\kb -> kb{kChar = char, kStrobe = 0xFF})
