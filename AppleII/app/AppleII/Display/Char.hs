module AppleII.Display.Char where

import AppleII.Memory
import Paths_AppleII (getDataFileName)
import Data.Word
import qualified Data.ByteString as B
import Control.Monad (when)

data CharacterBank = CharacterBank 
    {   bankRom :: Memory
    ,   bankBlink :: Bool }

data Character = CHR_AT
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
    when (expectedRomSize /= length romData) $ error "Font ROM has incorrect size"
    fromList ReadOnly romData

characterBank :: IO CharacterBank
characterBank = do
    romData <- loadFont
    return $ CharacterBank { bankRom = romData, bankBlink = False}

tick :: CharacterBank -> CharacterBank
tick cb = cb

getChar :: CharacterBank -> Word8 -> IO [[Word8]]
getChar bank charByte = getChar2 bank char mod where
    (char, mod) = parseByte charByte

getChar2 :: CharacterBank -> Character -> CharMod -> IO [[Word8]]
getChar2 bank char CHAR_MOD_NORMAL = getChar' bank char
getChar2 bank char CHAR_MOD_INVERTED = map (map (0xFF - )) <$> getChar' bank char
getChar2 bank char CHAR_MOD_FLASHING = if bankBlink bank then map (map (0xFF - ) ) <$> getChar' bank char else getChar' bank char

getChar' :: CharacterBank -> Character -> IO [[Word8]]
getChar' bank char = undefined

parseByte :: Word8 -> (Character, CharMod)
parseByte 0x00 = (CHR_AT             , CHAR_MOD_INVERTED)
parseByte 0x01 = (CHR_A              , CHAR_MOD_INVERTED)
parseByte 0x02 = (CHR_B              , CHAR_MOD_INVERTED)
parseByte 0x03 = (CHR_C              , CHAR_MOD_INVERTED)
parseByte 0x04 = (CHR_D              , CHAR_MOD_INVERTED)
parseByte 0x05 = (CHR_E              , CHAR_MOD_INVERTED)
parseByte 0x06 = (CHR_F              , CHAR_MOD_INVERTED)
parseByte 0x07 = (CHR_G              , CHAR_MOD_INVERTED)
parseByte 0x08 = (CHR_H              , CHAR_MOD_INVERTED)
parseByte 0x09 = (CHR_I              , CHAR_MOD_INVERTED)
parseByte 0x0A = (CHR_J              , CHAR_MOD_INVERTED)
parseByte 0x0B = (CHR_K              , CHAR_MOD_INVERTED)
parseByte 0x0C = (CHR_L              , CHAR_MOD_INVERTED)
parseByte 0x0D = (CHR_M              , CHAR_MOD_INVERTED)
parseByte 0x0E = (CHR_N              , CHAR_MOD_INVERTED)
parseByte 0x0F = (CHR_O              , CHAR_MOD_INVERTED)
parseByte 0x10 = (CHR_P              , CHAR_MOD_INVERTED)
parseByte 0x11 = (CHR_Q              , CHAR_MOD_INVERTED)
parseByte 0x12 = (CHR_R              , CHAR_MOD_INVERTED)
parseByte 0x13 = (CHR_S              , CHAR_MOD_INVERTED)
parseByte 0x14 = (CHR_T              , CHAR_MOD_INVERTED)
parseByte 0x15 = (CHR_U              , CHAR_MOD_INVERTED)
parseByte 0x16 = (CHR_V              , CHAR_MOD_INVERTED)
parseByte 0x17 = (CHR_W              , CHAR_MOD_INVERTED)
parseByte 0x18 = (CHR_X              , CHAR_MOD_INVERTED)
parseByte 0x19 = (CHR_Y              , CHAR_MOD_INVERTED)
parseByte 0x1A = (CHR_Z              , CHAR_MOD_INVERTED)
parseByte 0x1B = (CHAR_LBRACK        , CHAR_MOD_INVERTED)
parseByte 0x1C = (CHAR_ISLASH        , CHAR_MOD_INVERTED)
parseByte 0x1D = (CHAR_RBRACK        , CHAR_MOD_INVERTED)
parseByte 0x1E = (CHAR_CARET         , CHAR_MOD_INVERTED)
parseByte 0x1F = (CHAR_UNDERSCORE    , CHAR_MOD_INVERTED)
parseByte 0x20 = (CHAR_SPACE         , CHAR_MOD_INVERTED)
parseByte 0x21 = (CHAR_EXCLAM        , CHAR_MOD_INVERTED)
parseByte 0x22 = (CHAR_QUOTE         , CHAR_MOD_INVERTED)
parseByte 0x23 = (CHAR_HASH          , CHAR_MOD_INVERTED)
parseByte 0x24 = (CHAR_DOLLAR        , CHAR_MOD_INVERTED)
parseByte 0x25 = (CHAR_PERCENT       , CHAR_MOD_INVERTED)
parseByte 0x26 = (CHAR_AMP           , CHAR_MOD_INVERTED)
parseByte 0x27 = (CHAR_APOS          , CHAR_MOD_INVERTED)
parseByte 0x28 = (CHAR_LPAREN        , CHAR_MOD_INVERTED)
parseByte 0x29 = (CHAR_RPAREN        , CHAR_MOD_INVERTED)
parseByte 0x2A = (CHAR_STAR          , CHAR_MOD_INVERTED)
parseByte 0x2B = (CHAR_PLUS          , CHAR_MOD_INVERTED)
parseByte 0x2C = (CHAR_COMMA         , CHAR_MOD_INVERTED)
parseByte 0x2D = (CHAR_MINUS         , CHAR_MOD_INVERTED)
parseByte 0x2E = (CHAR_PERIOD        , CHAR_MOD_INVERTED)
parseByte 0x2F = (CHAR_SLASH         , CHAR_MOD_INVERTED)
parseByte 0x30 = (CHAR_0             , CHAR_MOD_INVERTED)
parseByte 0x31 = (CHAR_1             , CHAR_MOD_INVERTED)
parseByte 0x32 = (CHAR_2             , CHAR_MOD_INVERTED)
parseByte 0x33 = (CHAR_3             , CHAR_MOD_INVERTED)
parseByte 0x34 = (CHAR_4             , CHAR_MOD_INVERTED)
parseByte 0x35 = (CHAR_5             , CHAR_MOD_INVERTED)
parseByte 0x36 = (CHAR_6             , CHAR_MOD_INVERTED)
parseByte 0x37 = (CHAR_7             , CHAR_MOD_INVERTED)
parseByte 0x38 = (CHAR_8             , CHAR_MOD_INVERTED)
parseByte 0x39 = (CHAR_9             , CHAR_MOD_INVERTED)
parseByte 0x3A = (CHAR_COLON         , CHAR_MOD_INVERTED)
parseByte 0x3B = (CHAR_SEMI          , CHAR_MOD_INVERTED)
parseByte 0x3C = (CHAR_LT            , CHAR_MOD_INVERTED)
parseByte 0x3D = (CHAR_EQ            , CHAR_MOD_INVERTED)
parseByte 0x3E = (CHAR_GT            , CHAR_MOD_INVERTED)
parseByte 0x3F = (CHAR_QUESTION      , CHAR_MOD_INVERTED)

parseByte 0x40 = (CHR_AT             , CHAR_MOD_FLASHING)
parseByte 0x41 = (CHR_A              , CHAR_MOD_FLASHING)
parseByte 0x42 = (CHR_B              , CHAR_MOD_FLASHING)
parseByte 0x43 = (CHR_C              , CHAR_MOD_FLASHING)
parseByte 0x44 = (CHR_D              , CHAR_MOD_FLASHING)
parseByte 0x45 = (CHR_E              , CHAR_MOD_FLASHING)
parseByte 0x46 = (CHR_F              , CHAR_MOD_FLASHING)
parseByte 0x47 = (CHR_G              , CHAR_MOD_FLASHING)
parseByte 0x48 = (CHR_H              , CHAR_MOD_FLASHING)
parseByte 0x49 = (CHR_I              , CHAR_MOD_FLASHING)
parseByte 0x4A = (CHR_J              , CHAR_MOD_FLASHING)
parseByte 0x4B = (CHR_K              , CHAR_MOD_FLASHING)
parseByte 0x4C = (CHR_L              , CHAR_MOD_FLASHING)
parseByte 0x4D = (CHR_M              , CHAR_MOD_FLASHING)
parseByte 0x4E = (CHR_N              , CHAR_MOD_FLASHING)
parseByte 0x4F = (CHR_O              , CHAR_MOD_FLASHING)
parseByte 0x50 = (CHR_P              , CHAR_MOD_FLASHING)
parseByte 0x51 = (CHR_Q              , CHAR_MOD_FLASHING)
parseByte 0x52 = (CHR_R              , CHAR_MOD_FLASHING)
parseByte 0x53 = (CHR_S              , CHAR_MOD_FLASHING)
parseByte 0x54 = (CHR_T              , CHAR_MOD_FLASHING)
parseByte 0x55 = (CHR_U              , CHAR_MOD_FLASHING)
parseByte 0x56 = (CHR_V              , CHAR_MOD_FLASHING)
parseByte 0x57 = (CHR_W              , CHAR_MOD_FLASHING)
parseByte 0x58 = (CHR_X              , CHAR_MOD_FLASHING)
parseByte 0x59 = (CHR_Y              , CHAR_MOD_FLASHING)
parseByte 0x5A = (CHR_Z              , CHAR_MOD_FLASHING)
parseByte 0x5B = (CHAR_LBRACK        , CHAR_MOD_FLASHING)
parseByte 0x5C = (CHAR_ISLASH        , CHAR_MOD_FLASHING)
parseByte 0x5D = (CHAR_RBRACK        , CHAR_MOD_FLASHING)
parseByte 0x5E = (CHAR_CARET         , CHAR_MOD_FLASHING)
parseByte 0x5F = (CHAR_UNDERSCORE    , CHAR_MOD_FLASHING)
parseByte 0x60 = (CHAR_SPACE         , CHAR_MOD_FLASHING)
parseByte 0x61 = (CHAR_EXCLAM        , CHAR_MOD_FLASHING)
parseByte 0x62 = (CHAR_QUOTE         , CHAR_MOD_FLASHING)
parseByte 0x63 = (CHAR_HASH          , CHAR_MOD_FLASHING)
parseByte 0x64 = (CHAR_DOLLAR        , CHAR_MOD_FLASHING)
parseByte 0x65 = (CHAR_PERCENT       , CHAR_MOD_FLASHING)
parseByte 0x66 = (CHAR_AMP           , CHAR_MOD_FLASHING)
parseByte 0x67 = (CHAR_APOS          , CHAR_MOD_FLASHING)
parseByte 0x68 = (CHAR_LPAREN        , CHAR_MOD_FLASHING)
parseByte 0x69 = (CHAR_RPAREN        , CHAR_MOD_FLASHING)
parseByte 0x6A = (CHAR_STAR          , CHAR_MOD_FLASHING)
parseByte 0x6B = (CHAR_PLUS          , CHAR_MOD_FLASHING)
parseByte 0x6C = (CHAR_COMMA         , CHAR_MOD_FLASHING)
parseByte 0x6D = (CHAR_MINUS         , CHAR_MOD_FLASHING)
parseByte 0x6E = (CHAR_PERIOD        , CHAR_MOD_FLASHING)
parseByte 0x6F = (CHAR_SLASH         , CHAR_MOD_FLASHING)
parseByte 0x70 = (CHAR_0             , CHAR_MOD_FLASHING)
parseByte 0x71 = (CHAR_1             , CHAR_MOD_FLASHING)
parseByte 0x72 = (CHAR_2             , CHAR_MOD_FLASHING)
parseByte 0x73 = (CHAR_3             , CHAR_MOD_FLASHING)
parseByte 0x74 = (CHAR_4             , CHAR_MOD_FLASHING)
parseByte 0x75 = (CHAR_5             , CHAR_MOD_FLASHING)
parseByte 0x76 = (CHAR_6             , CHAR_MOD_FLASHING)
parseByte 0x77 = (CHAR_7             , CHAR_MOD_FLASHING)
parseByte 0x78 = (CHAR_8             , CHAR_MOD_FLASHING)
parseByte 0x79 = (CHAR_9             , CHAR_MOD_FLASHING)
parseByte 0x7A = (CHAR_COLON         , CHAR_MOD_FLASHING)
parseByte 0x7B = (CHAR_SEMI          , CHAR_MOD_FLASHING)
parseByte 0x7C = (CHAR_LT            , CHAR_MOD_FLASHING)
parseByte 0x7D = (CHAR_EQ            , CHAR_MOD_FLASHING)
parseByte 0x7E = (CHAR_GT            , CHAR_MOD_FLASHING)
parseByte 0x7F = (CHAR_QUESTION      , CHAR_MOD_FLASHING)

parseByte 0x80 = (CHR_AT             , CHAR_MOD_NORMAL)
parseByte 0x81 = (CHR_A              , CHAR_MOD_NORMAL)
parseByte 0x82 = (CHR_B              , CHAR_MOD_NORMAL)
parseByte 0x83 = (CHR_C              , CHAR_MOD_NORMAL)
parseByte 0x84 = (CHR_D              , CHAR_MOD_NORMAL)
parseByte 0x85 = (CHR_E              , CHAR_MOD_NORMAL)
parseByte 0x86 = (CHR_F              , CHAR_MOD_NORMAL)
parseByte 0x87 = (CHR_G              , CHAR_MOD_NORMAL)
parseByte 0x88 = (CHR_H              , CHAR_MOD_NORMAL)
parseByte 0x89 = (CHR_I              , CHAR_MOD_NORMAL)
parseByte 0x8A = (CHR_J              , CHAR_MOD_NORMAL)
parseByte 0x8B = (CHR_K              , CHAR_MOD_NORMAL)
parseByte 0x8C = (CHR_L              , CHAR_MOD_NORMAL)
parseByte 0x8D = (CHR_M              , CHAR_MOD_NORMAL)
parseByte 0x8E = (CHR_N              , CHAR_MOD_NORMAL)
parseByte 0x8F = (CHR_O              , CHAR_MOD_NORMAL)
parseByte 0x90 = (CHR_P              , CHAR_MOD_NORMAL)
parseByte 0x91 = (CHR_Q              , CHAR_MOD_NORMAL)
parseByte 0x92 = (CHR_R              , CHAR_MOD_NORMAL)
parseByte 0x93 = (CHR_S              , CHAR_MOD_NORMAL)
parseByte 0x94 = (CHR_T              , CHAR_MOD_NORMAL)
parseByte 0x95 = (CHR_U              , CHAR_MOD_NORMAL)
parseByte 0x96 = (CHR_V              , CHAR_MOD_NORMAL)
parseByte 0x97 = (CHR_W              , CHAR_MOD_NORMAL)
parseByte 0x98 = (CHR_X              , CHAR_MOD_NORMAL)
parseByte 0x99 = (CHR_Y              , CHAR_MOD_NORMAL)
parseByte 0x9A = (CHR_Z              , CHAR_MOD_NORMAL)
parseByte 0x9B = (CHAR_LBRACK        , CHAR_MOD_NORMAL)
parseByte 0x9C = (CHAR_ISLASH        , CHAR_MOD_NORMAL)
parseByte 0x9D = (CHAR_RBRACK        , CHAR_MOD_NORMAL)
parseByte 0x9E = (CHAR_CARET         , CHAR_MOD_NORMAL)
parseByte 0x9F = (CHAR_UNDERSCORE    , CHAR_MOD_NORMAL)
parseByte 0xA0 = (CHAR_SPACE         , CHAR_MOD_NORMAL)
parseByte 0xA1 = (CHAR_EXCLAM        , CHAR_MOD_NORMAL)
parseByte 0xA2 = (CHAR_QUOTE         , CHAR_MOD_NORMAL)
parseByte 0xA3 = (CHAR_HASH          , CHAR_MOD_NORMAL)
parseByte 0xA4 = (CHAR_DOLLAR        , CHAR_MOD_NORMAL)
parseByte 0xA5 = (CHAR_PERCENT       , CHAR_MOD_NORMAL)
parseByte 0xA6 = (CHAR_AMP           , CHAR_MOD_NORMAL)
parseByte 0xA7 = (CHAR_APOS          , CHAR_MOD_NORMAL)
parseByte 0xA8 = (CHAR_LPAREN        , CHAR_MOD_NORMAL)
parseByte 0xA9 = (CHAR_RPAREN        , CHAR_MOD_NORMAL)
parseByte 0xAA = (CHAR_STAR          , CHAR_MOD_NORMAL)
parseByte 0xAB = (CHAR_PLUS          , CHAR_MOD_NORMAL)
parseByte 0xAC = (CHAR_COMMA         , CHAR_MOD_NORMAL)
parseByte 0xAD = (CHAR_MINUS         , CHAR_MOD_NORMAL)
parseByte 0xAE = (CHAR_PERIOD        , CHAR_MOD_NORMAL)
parseByte 0xAF = (CHAR_SLASH         , CHAR_MOD_NORMAL)
parseByte 0xB0 = (CHAR_0             , CHAR_MOD_NORMAL)
parseByte 0xB1 = (CHAR_1             , CHAR_MOD_NORMAL)
parseByte 0xB2 = (CHAR_2             , CHAR_MOD_NORMAL)
parseByte 0xB3 = (CHAR_3             , CHAR_MOD_NORMAL)
parseByte 0xB4 = (CHAR_4             , CHAR_MOD_NORMAL)
parseByte 0xB5 = (CHAR_5             , CHAR_MOD_NORMAL)
parseByte 0xB6 = (CHAR_6             , CHAR_MOD_NORMAL)
parseByte 0xB7 = (CHAR_7             , CHAR_MOD_NORMAL)
parseByte 0xB8 = (CHAR_8             , CHAR_MOD_NORMAL)
parseByte 0xB9 = (CHAR_9             , CHAR_MOD_NORMAL)
parseByte 0xBA = (CHAR_COLON         , CHAR_MOD_NORMAL)
parseByte 0xBB = (CHAR_SEMI          , CHAR_MOD_NORMAL)
parseByte 0xBC = (CHAR_LT            , CHAR_MOD_NORMAL)
parseByte 0xBD = (CHAR_EQ            , CHAR_MOD_NORMAL)
parseByte 0xBE = (CHAR_GT            , CHAR_MOD_NORMAL)
parseByte 0xBF = (CHAR_QUESTION      , CHAR_MOD_NORMAL)

-- Check this. I'm not certain what happens here.
parseByte 0xC0 = (CHR_AT             , CHAR_MOD_NORMAL)
parseByte 0xC1 = (CHR_A              , CHAR_MOD_NORMAL)
parseByte 0xC2 = (CHR_B              , CHAR_MOD_NORMAL)
parseByte 0xC3 = (CHR_C              , CHAR_MOD_NORMAL)
parseByte 0xC4 = (CHR_D              , CHAR_MOD_NORMAL)
parseByte 0xC5 = (CHR_E              , CHAR_MOD_NORMAL)
parseByte 0xC6 = (CHR_F              , CHAR_MOD_NORMAL)
parseByte 0xC7 = (CHR_G              , CHAR_MOD_NORMAL)
parseByte 0xC8 = (CHR_H              , CHAR_MOD_NORMAL)
parseByte 0xC9 = (CHR_I              , CHAR_MOD_NORMAL)
parseByte 0xCA = (CHR_J              , CHAR_MOD_NORMAL)
parseByte 0xCB = (CHR_K              , CHAR_MOD_NORMAL)
parseByte 0xCC = (CHR_L              , CHAR_MOD_NORMAL)
parseByte 0xCD = (CHR_M              , CHAR_MOD_NORMAL)
parseByte 0xCE = (CHR_N              , CHAR_MOD_NORMAL)
parseByte 0xCF = (CHR_O              , CHAR_MOD_NORMAL)
parseByte 0xD0 = (CHR_P              , CHAR_MOD_NORMAL)
parseByte 0xD1 = (CHR_Q              , CHAR_MOD_NORMAL)
parseByte 0xD2 = (CHR_R              , CHAR_MOD_NORMAL)
parseByte 0xD3 = (CHR_S              , CHAR_MOD_NORMAL)
parseByte 0xD4 = (CHR_T              , CHAR_MOD_NORMAL)
parseByte 0xD5 = (CHR_U              , CHAR_MOD_NORMAL)
parseByte 0xD6 = (CHR_V              , CHAR_MOD_NORMAL)
parseByte 0xD7 = (CHR_W              , CHAR_MOD_NORMAL)
parseByte 0xD8 = (CHR_X              , CHAR_MOD_NORMAL)
parseByte 0xD9 = (CHR_Y              , CHAR_MOD_NORMAL)
parseByte 0xDA = (CHR_Z              , CHAR_MOD_NORMAL)
parseByte 0xDB = (CHAR_LBRACK        , CHAR_MOD_NORMAL)
parseByte 0xDC = (CHAR_ISLASH        , CHAR_MOD_NORMAL)
parseByte 0xDD = (CHAR_RBRACK        , CHAR_MOD_NORMAL)
parseByte 0xDE = (CHAR_CARET         , CHAR_MOD_NORMAL)
parseByte 0xDF = (CHAR_UNDERSCORE    , CHAR_MOD_NORMAL)
parseByte 0xE0 = (CHAR_SPACE         , CHAR_MOD_NORMAL)
parseByte 0xE1 = (CHAR_EXCLAM        , CHAR_MOD_NORMAL)
parseByte 0xE2 = (CHAR_QUOTE         , CHAR_MOD_NORMAL)
parseByte 0xE3 = (CHAR_HASH          , CHAR_MOD_NORMAL)
parseByte 0xE4 = (CHAR_DOLLAR        , CHAR_MOD_NORMAL)
parseByte 0xE5 = (CHAR_PERCENT       , CHAR_MOD_NORMAL)
parseByte 0xE6 = (CHAR_AMP           , CHAR_MOD_NORMAL)
parseByte 0xE7 = (CHAR_APOS          , CHAR_MOD_NORMAL)
parseByte 0xE8 = (CHAR_LPAREN        , CHAR_MOD_NORMAL)
parseByte 0xE9 = (CHAR_RPAREN        , CHAR_MOD_NORMAL)
parseByte 0xEA = (CHAR_STAR          , CHAR_MOD_NORMAL)
parseByte 0xEB = (CHAR_PLUS          , CHAR_MOD_NORMAL)
parseByte 0xEC = (CHAR_COMMA         , CHAR_MOD_NORMAL)
parseByte 0xED = (CHAR_MINUS         , CHAR_MOD_NORMAL)
parseByte 0xEE = (CHAR_PERIOD        , CHAR_MOD_NORMAL)
parseByte 0xEF = (CHAR_SLASH         , CHAR_MOD_NORMAL)
parseByte 0xF0 = (CHAR_0             , CHAR_MOD_NORMAL)
parseByte 0xF1 = (CHAR_1             , CHAR_MOD_NORMAL)
parseByte 0xF2 = (CHAR_2             , CHAR_MOD_NORMAL)
parseByte 0xF3 = (CHAR_3             , CHAR_MOD_NORMAL)
parseByte 0xF4 = (CHAR_4             , CHAR_MOD_NORMAL)
parseByte 0xF5 = (CHAR_5             , CHAR_MOD_NORMAL)
parseByte 0xF6 = (CHAR_6             , CHAR_MOD_NORMAL)
parseByte 0xF7 = (CHAR_7             , CHAR_MOD_NORMAL)
parseByte 0xF8 = (CHAR_8             , CHAR_MOD_NORMAL)
parseByte 0xF9 = (CHAR_9             , CHAR_MOD_NORMAL)
parseByte 0xFA = (CHAR_COLON         , CHAR_MOD_NORMAL)
parseByte 0xFB = (CHAR_SEMI          , CHAR_MOD_NORMAL)
parseByte 0xFC = (CHAR_LT            , CHAR_MOD_NORMAL)
parseByte 0xFD = (CHAR_EQ            , CHAR_MOD_NORMAL)
parseByte 0xFE = (CHAR_GT            , CHAR_MOD_NORMAL)
parseByte 0xFF = (CHAR_QUESTION      , CHAR_MOD_NORMAL)


-- Position of char in ROM
charAddress :: Character -> Int
charAddress CHR_AT            = 0x00 * 7 * 8
charAddress CHR_A             = 0x01 * 7 * 8
charAddress CHR_B             = 0x02 * 7 * 8
charAddress CHR_C             = 0x03 * 7 * 8
charAddress CHR_D             = 0x04 * 7 * 8
charAddress CHR_E             = 0x05 * 7 * 8
charAddress CHR_F             = 0x06 * 7 * 8
charAddress CHR_G             = 0x07 * 7 * 8
charAddress CHR_H             = 0x08 * 7 * 8
charAddress CHR_I             = 0x09 * 7 * 8
charAddress CHR_J             = 0x0A * 7 * 8
charAddress CHR_K             = 0x0B * 7 * 8
charAddress CHR_L             = 0x0C * 7 * 8
charAddress CHR_M             = 0x0D * 7 * 8
charAddress CHR_N             = 0x0E * 7 * 8
charAddress CHR_O             = 0x0F * 7 * 8
charAddress CHR_P             = 0x10 * 7 * 8
charAddress CHR_Q             = 0x11 * 7 * 8
charAddress CHR_R             = 0x12 * 7 * 8
charAddress CHR_S             = 0x13 * 7 * 8
charAddress CHR_T             = 0x14 * 7 * 8
charAddress CHR_U             = 0x15 * 7 * 8
charAddress CHR_V             = 0x16 * 7 * 8
charAddress CHR_W             = 0x17 * 7 * 8
charAddress CHR_X             = 0x18 * 7 * 8
charAddress CHR_Y             = 0x19 * 7 * 8
charAddress CHR_Z             = 0x1A * 7 * 8
charAddress CHAR_LBRACK       = 0x1B * 7 * 8
charAddress CHAR_ISLASH       = 0x1C * 7 * 8
charAddress CHAR_RBRACK       = 0x1D * 7 * 8
charAddress CHAR_CARET        = 0x1E * 7 * 8
charAddress CHAR_UNDERSCORE   = 0x1F * 7 * 8
charAddress CHAR_SPACE        = 0x20 * 7 * 8
charAddress CHAR_EXCLAM       = 0x21 * 7 * 8
charAddress CHAR_QUOTE        = 0x22 * 7 * 8
charAddress CHAR_HASH         = 0x23 * 7 * 8
charAddress CHAR_DOLLAR       = 0x24 * 7 * 8
charAddress CHAR_PERCENT      = 0x25 * 7 * 8
charAddress CHAR_AMP          = 0x26 * 7 * 8
charAddress CHAR_APOS         = 0x27 * 7 * 8
charAddress CHAR_LPAREN       = 0x28 * 7 * 8
charAddress CHAR_RPAREN       = 0x29 * 7 * 8
charAddress CHAR_STAR         = 0x2A * 7 * 8
charAddress CHAR_PLUS         = 0x2B * 7 * 8
charAddress CHAR_COMMA        = 0x2C * 7 * 8
charAddress CHAR_MINUS        = 0x2D * 7 * 8
charAddress CHAR_PERIOD       = 0x2E * 7 * 8
charAddress CHAR_SLASH        = 0x2F * 7 * 8
charAddress CHAR_0            = 0x30 * 7 * 8
charAddress CHAR_1            = 0x31 * 7 * 8
charAddress CHAR_2            = 0x32 * 7 * 8
charAddress CHAR_3            = 0x33 * 7 * 8
charAddress CHAR_4            = 0x34 * 7 * 8
charAddress CHAR_5            = 0x35 * 7 * 8
charAddress CHAR_6            = 0x36 * 7 * 8
charAddress CHAR_7            = 0x37 * 7 * 8
charAddress CHAR_8            = 0x38 * 7 * 8
charAddress CHAR_9            = 0x39 * 7 * 8
charAddress CHAR_COLON        = 0x3A * 7 * 8
charAddress CHAR_SEMI         = 0x3B * 7 * 8
charAddress CHAR_LT           = 0x3C * 7 * 8
charAddress CHAR_EQ           = 0x3D * 7 * 8
charAddress CHAR_GT           = 0x3E * 7 * 8
charAddress CHAR_QUESTION     = 0x3F * 7 * 8
