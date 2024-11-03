module AppleII.Keyboard where

import Data.Word
import Control.Monad.State
import Control.Monad (when)

newtype Keyboard = Keyboard {kChar :: Word8}

new :: Keyboard
new = Keyboard {kChar = 0}

readChar :: Keyboard -> Word8
readChar = kChar

writeChar' :: Word8 -> State Keyboard ()
writeChar' char = modify (\kb -> kb{kChar = char})

writeChar :: Word8 -> Keyboard -> Keyboard
writeChar char = execState (writeChar' char)

clearStrobe' :: State Keyboard ()
clearStrobe' = do
    char <- gets kChar
    when (char >= 0x80) (modify (\k -> k{kChar = char - 0x80}))

clearStrobe :: Keyboard -> Keyboard
clearStrobe = execState clearStrobe'

