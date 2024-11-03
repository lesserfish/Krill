module AppleII.Display.Char (
  CharacterBank(..)
, characterBank
, tick
, getGlyph
) where

import AppleII.Memory
import Paths_AppleII (getDataFileName)
import Data.Word
import qualified Data.ByteString as B
import Control.Monad (when)

data CharacterBank = CharacterBank 
    {   bankRom :: Memory
    ,   bankBlink :: Bool
    ,   bankCounter :: Int }

data Character = CHAR_AT
               | CHAR_A
               | CHAR_B
               | CHAR_C
               | CHAR_D
               | CHAR_E
               | CHAR_F
               | CHAR_G
               | CHAR_H
               | CHAR_I
               | CHAR_J
               | CHAR_K
               | CHAR_L
               | CHAR_M
               | CHAR_N
               | CHAR_O
               | CHAR_P
               | CHAR_Q
               | CHAR_R
               | CHAR_S
               | CHAR_T
               | CHAR_U
               | CHAR_V
               | CHAR_W
               | CHAR_X
               | CHAR_Y
               | CHAR_Z
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
    return $ CharacterBank { bankRom = romData, bankBlink = False, bankCounter = 0}

tick :: CharacterBank -> IO CharacterBank
tick cb = do
    let counter = bankCounter cb
    -- TODO: Fix this magic number.
    let cb' = if counter > 80000 then cb {bankCounter = 0, bankBlink = not (bankBlink cb)} else cb {bankCounter = bankCounter cb + 1 }
    return cb'

getGlyph :: CharacterBank -> Word8 -> IO [[Word8]]
getGlyph bank charByte = getGlyph2 bank char cmod where
    (char, cmod) = parseByte charByte

getGlyph2 :: CharacterBank -> Character -> CharMod -> IO [[Word8]]
getGlyph2 bank char CHAR_MOD_NORMAL = getGlyph' bank char
getGlyph2 bank char CHAR_MOD_INVERTED = map (map (0xFF - )) <$> getGlyph' bank char
getGlyph2 bank char CHAR_MOD_FLASHING = if bankBlink bank then map (map (0xFF - ) ) <$> getGlyph' bank char else getGlyph' bank char

getGlyph' :: CharacterBank -> Character -> IO [[Word8]]
getGlyph' bank char = do
    let rom = bankRom bank
    let baseAddress = charAddress char
    mapM (\y -> do
        mapM (\x -> do
            let address = fromIntegral (baseAddress + 8*x + y)
            (\z -> 0xFF - z * 0xFF) <$> readByte rom address 
            ) [0..6]
        ) [0..7]


parseByte :: Word8 -> (Character, CharMod)
parseByte 0x00 = (CHAR_AT             , CHAR_MOD_INVERTED)
parseByte 0x01 = (CHAR_A              , CHAR_MOD_INVERTED)
parseByte 0x02 = (CHAR_B              , CHAR_MOD_INVERTED)
parseByte 0x03 = (CHAR_C              , CHAR_MOD_INVERTED)
parseByte 0x04 = (CHAR_D              , CHAR_MOD_INVERTED)
parseByte 0x05 = (CHAR_E              , CHAR_MOD_INVERTED)
parseByte 0x06 = (CHAR_F              , CHAR_MOD_INVERTED)
parseByte 0x07 = (CHAR_G              , CHAR_MOD_INVERTED)
parseByte 0x08 = (CHAR_H              , CHAR_MOD_INVERTED)
parseByte 0x09 = (CHAR_I              , CHAR_MOD_INVERTED)
parseByte 0x0A = (CHAR_J              , CHAR_MOD_INVERTED)
parseByte 0x0B = (CHAR_K              , CHAR_MOD_INVERTED)
parseByte 0x0C = (CHAR_L              , CHAR_MOD_INVERTED)
parseByte 0x0D = (CHAR_M              , CHAR_MOD_INVERTED)
parseByte 0x0E = (CHAR_N              , CHAR_MOD_INVERTED)
parseByte 0x0F = (CHAR_O              , CHAR_MOD_INVERTED)
parseByte 0x10 = (CHAR_P              , CHAR_MOD_INVERTED)
parseByte 0x11 = (CHAR_Q              , CHAR_MOD_INVERTED)
parseByte 0x12 = (CHAR_R              , CHAR_MOD_INVERTED)
parseByte 0x13 = (CHAR_S              , CHAR_MOD_INVERTED)
parseByte 0x14 = (CHAR_T              , CHAR_MOD_INVERTED)
parseByte 0x15 = (CHAR_U              , CHAR_MOD_INVERTED)
parseByte 0x16 = (CHAR_V              , CHAR_MOD_INVERTED)
parseByte 0x17 = (CHAR_W              , CHAR_MOD_INVERTED)
parseByte 0x18 = (CHAR_X              , CHAR_MOD_INVERTED)
parseByte 0x19 = (CHAR_Y              , CHAR_MOD_INVERTED)
parseByte 0x1A = (CHAR_Z              , CHAR_MOD_INVERTED)
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

parseByte 0x40 = (CHAR_AT             , CHAR_MOD_FLASHING)
parseByte 0x41 = (CHAR_A              , CHAR_MOD_FLASHING)
parseByte 0x42 = (CHAR_B              , CHAR_MOD_FLASHING)
parseByte 0x43 = (CHAR_C              , CHAR_MOD_FLASHING)
parseByte 0x44 = (CHAR_D              , CHAR_MOD_FLASHING)
parseByte 0x45 = (CHAR_E              , CHAR_MOD_FLASHING)
parseByte 0x46 = (CHAR_F              , CHAR_MOD_FLASHING)
parseByte 0x47 = (CHAR_G              , CHAR_MOD_FLASHING)
parseByte 0x48 = (CHAR_H              , CHAR_MOD_FLASHING)
parseByte 0x49 = (CHAR_I              , CHAR_MOD_FLASHING)
parseByte 0x4A = (CHAR_J              , CHAR_MOD_FLASHING)
parseByte 0x4B = (CHAR_K              , CHAR_MOD_FLASHING)
parseByte 0x4C = (CHAR_L              , CHAR_MOD_FLASHING)
parseByte 0x4D = (CHAR_M              , CHAR_MOD_FLASHING)
parseByte 0x4E = (CHAR_N              , CHAR_MOD_FLASHING)
parseByte 0x4F = (CHAR_O              , CHAR_MOD_FLASHING)
parseByte 0x50 = (CHAR_P              , CHAR_MOD_FLASHING)
parseByte 0x51 = (CHAR_Q              , CHAR_MOD_FLASHING)
parseByte 0x52 = (CHAR_R              , CHAR_MOD_FLASHING)
parseByte 0x53 = (CHAR_S              , CHAR_MOD_FLASHING)
parseByte 0x54 = (CHAR_T              , CHAR_MOD_FLASHING)
parseByte 0x55 = (CHAR_U              , CHAR_MOD_FLASHING)
parseByte 0x56 = (CHAR_V              , CHAR_MOD_FLASHING)
parseByte 0x57 = (CHAR_W              , CHAR_MOD_FLASHING)
parseByte 0x58 = (CHAR_X              , CHAR_MOD_FLASHING)
parseByte 0x59 = (CHAR_Y              , CHAR_MOD_FLASHING)
parseByte 0x5A = (CHAR_Z              , CHAR_MOD_FLASHING)
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

parseByte 0x80 = (CHAR_AT             , CHAR_MOD_NORMAL)
parseByte 0x81 = (CHAR_A              , CHAR_MOD_NORMAL)
parseByte 0x82 = (CHAR_B              , CHAR_MOD_NORMAL)
parseByte 0x83 = (CHAR_C              , CHAR_MOD_NORMAL)
parseByte 0x84 = (CHAR_D              , CHAR_MOD_NORMAL)
parseByte 0x85 = (CHAR_E              , CHAR_MOD_NORMAL)
parseByte 0x86 = (CHAR_F              , CHAR_MOD_NORMAL)
parseByte 0x87 = (CHAR_G              , CHAR_MOD_NORMAL)
parseByte 0x88 = (CHAR_H              , CHAR_MOD_NORMAL)
parseByte 0x89 = (CHAR_I              , CHAR_MOD_NORMAL)
parseByte 0x8A = (CHAR_J              , CHAR_MOD_NORMAL)
parseByte 0x8B = (CHAR_K              , CHAR_MOD_NORMAL)
parseByte 0x8C = (CHAR_L              , CHAR_MOD_NORMAL)
parseByte 0x8D = (CHAR_M              , CHAR_MOD_NORMAL)
parseByte 0x8E = (CHAR_N              , CHAR_MOD_NORMAL)
parseByte 0x8F = (CHAR_O              , CHAR_MOD_NORMAL)
parseByte 0x90 = (CHAR_P              , CHAR_MOD_NORMAL)
parseByte 0x91 = (CHAR_Q              , CHAR_MOD_NORMAL)
parseByte 0x92 = (CHAR_R              , CHAR_MOD_NORMAL)
parseByte 0x93 = (CHAR_S              , CHAR_MOD_NORMAL)
parseByte 0x94 = (CHAR_T              , CHAR_MOD_NORMAL)
parseByte 0x95 = (CHAR_U              , CHAR_MOD_NORMAL)
parseByte 0x96 = (CHAR_V              , CHAR_MOD_NORMAL)
parseByte 0x97 = (CHAR_W              , CHAR_MOD_NORMAL)
parseByte 0x98 = (CHAR_X              , CHAR_MOD_NORMAL)
parseByte 0x99 = (CHAR_Y              , CHAR_MOD_NORMAL)
parseByte 0x9A = (CHAR_Z              , CHAR_MOD_NORMAL)
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
parseByte 0xC0 = (CHAR_AT             , CHAR_MOD_NORMAL)
parseByte 0xC1 = (CHAR_A              , CHAR_MOD_NORMAL)
parseByte 0xC2 = (CHAR_B              , CHAR_MOD_NORMAL)
parseByte 0xC3 = (CHAR_C              , CHAR_MOD_NORMAL)
parseByte 0xC4 = (CHAR_D              , CHAR_MOD_NORMAL)
parseByte 0xC5 = (CHAR_E              , CHAR_MOD_NORMAL)
parseByte 0xC6 = (CHAR_F              , CHAR_MOD_NORMAL)
parseByte 0xC7 = (CHAR_G              , CHAR_MOD_NORMAL)
parseByte 0xC8 = (CHAR_H              , CHAR_MOD_NORMAL)
parseByte 0xC9 = (CHAR_I              , CHAR_MOD_NORMAL)
parseByte 0xCA = (CHAR_J              , CHAR_MOD_NORMAL)
parseByte 0xCB = (CHAR_K              , CHAR_MOD_NORMAL)
parseByte 0xCC = (CHAR_L              , CHAR_MOD_NORMAL)
parseByte 0xCD = (CHAR_M              , CHAR_MOD_NORMAL)
parseByte 0xCE = (CHAR_N              , CHAR_MOD_NORMAL)
parseByte 0xCF = (CHAR_O              , CHAR_MOD_NORMAL)
parseByte 0xD0 = (CHAR_P              , CHAR_MOD_NORMAL)
parseByte 0xD1 = (CHAR_Q              , CHAR_MOD_NORMAL)
parseByte 0xD2 = (CHAR_R              , CHAR_MOD_NORMAL)
parseByte 0xD3 = (CHAR_S              , CHAR_MOD_NORMAL)
parseByte 0xD4 = (CHAR_T              , CHAR_MOD_NORMAL)
parseByte 0xD5 = (CHAR_U              , CHAR_MOD_NORMAL)
parseByte 0xD6 = (CHAR_V              , CHAR_MOD_NORMAL)
parseByte 0xD7 = (CHAR_W              , CHAR_MOD_NORMAL)
parseByte 0xD8 = (CHAR_X              , CHAR_MOD_NORMAL)
parseByte 0xD9 = (CHAR_Y              , CHAR_MOD_NORMAL)
parseByte 0xDA = (CHAR_Z              , CHAR_MOD_NORMAL)
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
parseByte _ = error "Undefined byte in parseByte"


-- Position of char in ROM
charAddress :: Character -> Int
charAddress CHAR_AT            = 0x00 * 7 * 8
charAddress CHAR_A             = 0x01 * 7 * 8
charAddress CHAR_B             = 0x02 * 7 * 8
charAddress CHAR_C             = 0x03 * 7 * 8
charAddress CHAR_D             = 0x04 * 7 * 8
charAddress CHAR_E             = 0x05 * 7 * 8
charAddress CHAR_F             = 0x06 * 7 * 8
charAddress CHAR_G             = 0x07 * 7 * 8
charAddress CHAR_H             = 0x08 * 7 * 8
charAddress CHAR_I             = 0x09 * 7 * 8
charAddress CHAR_J             = 0x0A * 7 * 8
charAddress CHAR_K             = 0x0B * 7 * 8
charAddress CHAR_L             = 0x0C * 7 * 8
charAddress CHAR_M             = 0x0D * 7 * 8
charAddress CHAR_N             = 0x0E * 7 * 8
charAddress CHAR_O             = 0x0F * 7 * 8
charAddress CHAR_P             = 0x10 * 7 * 8
charAddress CHAR_Q             = 0x11 * 7 * 8
charAddress CHAR_R             = 0x12 * 7 * 8
charAddress CHAR_S             = 0x13 * 7 * 8
charAddress CHAR_T             = 0x14 * 7 * 8
charAddress CHAR_U             = 0x15 * 7 * 8
charAddress CHAR_V             = 0x16 * 7 * 8
charAddress CHAR_W             = 0x17 * 7 * 8
charAddress CHAR_X             = 0x18 * 7 * 8
charAddress CHAR_Y             = 0x19 * 7 * 8
charAddress CHAR_Z             = 0x1A * 7 * 8
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
