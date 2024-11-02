module AppleII.Display.Blocks ( 
  RGB
, getBlock
, loadPalette
)where

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import AppleII.Memory
import Paths_AppleII (getDataFileName)
import SDL.Raw (readBE16)
import Control.Monad (when)

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

readPalette :: Memory -> Int -> IO RGB
readPalette palette idx = do
    let baseAddress = fromIntegral $ idx * 3
    r <- readByte palette (baseAddress + 0)
    g <- readByte palette (baseAddress + 1)
    b <- readByte palette (baseAddress + 2)
    return (r, g, b)

toRGB :: Memory -> COLOR -> IO RGB
toRGB palette COL_BLACK       = readPalette palette 0 
toRGB palette COL_MAGENTA     = readPalette palette 1 
toRGB palette COL_DARK_BLUE   = readPalette palette 2 
toRGB palette COL_PURPLE      = readPalette palette 3 
toRGB palette COL_GREEN       = readPalette palette 4 
toRGB palette COL_GREY_1      = readPalette palette 5 
toRGB palette COL_MEDIUM_BLUE = readPalette palette 6 
toRGB palette COL_LIGHT_BLUE  = readPalette palette 7 
toRGB palette COL_BROWN       = readPalette palette 8 
toRGB palette COL_ORANGE      = readPalette palette 9 
toRGB palette COL_GREY_2      = readPalette palette 10 
toRGB palette COL_PINK        = readPalette palette 11 
toRGB palette COL_LIGHT_GREEN = readPalette palette 12 
toRGB palette COL_YELLOW      = readPalette palette 13 
toRGB palette COL_AQUAMARINE  = readPalette palette 14 
toRGB palette COL_WHITE       = readPalette palette 15 

getBlock :: Memory -> Word8 -> IO [[RGB]]
getBlock palette byte =  do
    let (upperColor, lowerColor) = parseByte byte
    upperLine <- replicate 7 <$> toRGB palette upperColor
    lowerLine <- replicate 7 <$> toRGB palette lowerColor
    let upperBlock = replicate 4 upperLine
    let lowerBlock = replicate 4 lowerLine
    return $ upperBlock ++ lowerBlock

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

loadPalette :: IO Memory
loadPalette = do
    let expectedRomSize = 16 * 3
    filepath <- getDataFileName "Assets/palette.bin"
    romData <- B.unpack <$> B.readFile filepath :: IO [Word8]
    when (expectedRomSize /= length romData) $ error "Palette ROM has incorrect size"
    fromList ReadOnly romData
