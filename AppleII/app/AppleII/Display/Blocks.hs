module AppleII.Display.Blocks ( 
  RGB
, getBlock
)where

import Data.Word
import Data.Bits

type RGB = (Word8, Word8, Word8)

data COLOR = COL_BLACK
           | COL_MAGENTA
           | COL_DARK_BLUE
           | COL_PURPLE
           | COL_GREEN
           | COL_GREY_1
           | COL_MEDIUM_BLUE
           | COL_LIGHT_BLUE
           | COL_BROWN
           | COL_ORANGE
           | COL_GREY_2
           | COL_PINK
           | COL_LIGHT_GREEN
           | COL_YELLOW
           | COL_AQUAMARINE
           | COL_WHITE


toRGB :: COLOR -> RGB
toRGB COL_BLACK       = (  0,   0,   0)
toRGB COL_MAGENTA     = (221,   0,  51)
toRGB COL_DARK_BLUE   = (  0,   0, 153)
toRGB COL_PURPLE      = (221,  34, 221)
toRGB COL_GREEN       = (  0, 119,  34)
toRGB COL_GREY_1      = ( 85,  85,  85)
toRGB COL_MEDIUM_BLUE = ( 34,  34, 255)
toRGB COL_LIGHT_BLUE  = (102, 170, 255)
toRGB COL_BROWN       = (136,  85,   0)
toRGB COL_ORANGE      = (255, 102,   0)
toRGB COL_GREY_2      = (170, 170, 170)
toRGB COL_PINK        = (255, 153, 136)
toRGB COL_LIGHT_GREEN = ( 17, 221,   0)
toRGB COL_YELLOW      = (255, 255,   0)
toRGB COL_AQUAMARINE  = ( 68, 255, 153)
toRGB COL_WHITE       = (255, 255, 255)

getBlock :: Word8 -> [[RGB]]
getBlock byte =  upperBlock ++ lowerBlock where
    (upperColor, lowerColor) = parseByte byte
    upperLine = replicate 7 (toRGB upperColor)
    lowerLine = replicate 7 (toRGB lowerColor)
    upperBlock = replicate 4 upperLine
    lowerBlock = replicate 4 lowerLine

parseByte :: Word8 -> (COLOR, COLOR)
parseByte byte = (upperColor, lowerColor) where
    upperColor = parseNibble (byte .&. 0xF)
    lowerColor = parseNibble (byte .>>. 4)

parseNibble :: Word8 -> COLOR
parseNibble 0x0 = COL_BLACK
parseNibble 0x1 = COL_MAGENTA
parseNibble 0x2 = COL_DARK_BLUE
parseNibble 0x3 = COL_PURPLE
parseNibble 0x4 = COL_GREEN
parseNibble 0x5 = COL_GREY_1
parseNibble 0x6 = COL_MEDIUM_BLUE
parseNibble 0x7 = COL_LIGHT_BLUE
parseNibble 0x8 = COL_BROWN
parseNibble 0x9 = COL_ORANGE
parseNibble 0xA = COL_GREY_2
parseNibble 0xB = COL_PINK
parseNibble 0xC = COL_LIGHT_GREEN
parseNibble 0xD = COL_YELLOW
parseNibble 0xE = COL_AQUAMARINE
parseNibble 0xF = COL_WHITE
parseNibble _ = COL_BLACK
