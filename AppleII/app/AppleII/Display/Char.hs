module AppleII.Display.Char where

import AppleII.Memory
import Paths_AppleII (getDataFileName)
import Data.Word
import qualified Data.ByteString as B
import Control.Exception (assert)

data CharacterBank = CharacterBank 
    {   bankRom :: Memory
    ,   bankBlink :: Bool }

data Character = CHR_AT
               | CHR_LBRACE
               | CHR_RBRACE
               | CHAR_AT
               | CHR_A
               | CHR_B
               | CHR_C
               | CHR_D
               | CHR_E
               | CHR_F
               | CHR_G
               | CHR_H
               | CHR_I
               | CHR_J
               | CHR_K
               | CHR_L
               | CHR_M
               | CHR_N
               | CHR_O
               | CHR_P
               | CHR_Q
               | CHR_R
               | CHR_S
               | CHR_T
               | CHR_U
               | CHR_V
               | CHR_W
               | CHR_X
               | CHR_Y
               | CHR_Z
               | CHAR_LBRACK
               | CHAR_ISLASH
               | CHAR_RBRACK
               | CHAR_CARET
               | CHAR_UNDERSCORE
               | CHAR_SPACE
               | CHAR_EXCLAM
               | CHAR_QUOTE
               | CHAR_HASH
               | CHAR_DOLLAR
               | CHAR_PERCENT
               | CHAR_AMP
               | CHAR_APOS
               | CHAR_LPAREN
               | CHAR_RPAREN
               | CHAR_STAR
               | CHAR_PLUS
               | CHAR_COMMA
               | CHAR_MINUS
               | CHAR_PERIOD
               | CHAR_SLASH
               | CHAR_0
               | CHAR_1
               | CHAR_2
               | CHAR_3
               | CHAR_4
               | CHAR_5
               | CHAR_6
               | CHAR_7
               | CHAR_8
               | CHAR_9
               | CHAR_COLON
               | CHAR_SEMI
               | CHAR_LT
               | CHAR_EQ
               | CHAR_GT
               | CHAR_QUESTION

data CharMod = CHAR_MOD_NORMAL
          | CHAR_MOD_FLASHING
          | CHAR_MOD_INVERTED

loadFont :: IO Memory
loadFont = do
    let expectedRomSize = 64 * 7 * 8
    filepath <- getDataFileName "Assets/font.bin"
    romData <- B.unpack <$> B.readFile filepath :: IO [Word8]
    assert  (length romData == expectedRomSize) (fromList ReadOnly romData)

characterBank :: IO CharacterBank
characterBank = do
    romData <- loadFont
    return $ CharacterBank { bankRom = romData, bankBlink = False}

tick :: CharacterBank -> CharacterBank
tick cb = cb

getChar :: CharacterBank -> Character -> CharMod -> IO [[Word8]]
getChar bank char CHAR_MOD_NORMAL = getChar' bank char
getChar bank char CHAR_MOD_INVERTED = map (map (0xFF - )) <$> getChar' bank char
getChar bank char CHAR_MOD_FLASHING = if bankBlink bank then map (map (0xFF - ) ) <$> getChar' bank char else getChar' bank char

getChar' :: CharacterBank -> Character -> IO [[Word8]]
getChar' bank char = undefined
