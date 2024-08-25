{-# LANGUAGE OverloadedStrings #-}

module Frontend where

import qualified AppleI.Bus as AppleI
import Data.Text (Text)
import SDL hiding (get)
import Foreign.C.Types (CInt)
import Control.Monad.State
import Data.Word
import Data.Bits

_CHAR_WIDTH :: CInt 
_CHAR_WIDTH = 16

_CHAR_HEIGHT :: CInt 
_CHAR_HEIGHT = 16

data Context = Context {
    sdlWindow     :: Window,
    sdlRenderer   :: Renderer,
    fontTexture   :: Texture,
    windowTexture :: Texture,
    exitRequest   :: Bool,
    machine       :: AppleI.AppleI
}

initialize :: [Word8]  -> [Word8] -> FilePath -> IO Context
initialize biosData cassetteData fontFP = do
    initializeAll
    let winsize = 2 * V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    let texsize = V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    window <- createWindow "Apple I" defaultWindow{windowInitialSize =  winsize}
    renderer <- createRenderer window (-1) defaultRenderer{ rendererTargetTexture = True }
    fontSurf <- loadBMP fontFP
    fontText <- createTextureFromSurface renderer fontSurf
    winText <- createTexture renderer RGBA8888 TextureAccessTarget texsize
    m <- AppleI.new biosData cassetteData
    AppleI.reset m
    return Context {  sdlWindow = window
                    , sdlRenderer = renderer
                    , fontTexture = fontText
                    , windowTexture = winText
                    , exitRequest = False
                    , machine = m}

fontChar :: Word8 -> Rectangle CInt
fontChar byte = Rectangle (P $ V2 x y) (V2 _CHAR_WIDTH _CHAR_HEIGHT) where
    x = ((fromIntegral byte .&. 0x0F) .>>. 0) * _CHAR_WIDTH
    y = ((fromIntegral byte .&. 0x3F) .>>. 4) * _CHAR_HEIGHT

screenRect :: (Int, Int) -> Rectangle CInt
screenRect (a, b) = Rectangle (P $ V2 x y) (V2 _CHAR_WIDTH _CHAR_HEIGHT) where
    x = fromIntegral a * _CHAR_WIDTH
    y = fromIntegral b * _CHAR_HEIGHT


renderVBuffer :: [Word8] -> StateT Context IO ()
renderVBuffer buffer = do
    ctx <- get
    let fontTex = fontTexture ctx 
    let winText = windowTexture ctx
    let renderer = sdlRenderer ctx
    let pixels = [(x, y) | x <- [0..39], y <- [0..23]]
    rendererRenderTarget renderer $= Just winText
    clear renderer

    mapM_ (\(x, y) -> do
        let byte = buffer !! (y * 40 + x)
        let src_rect = fontChar byte
        let tgt_rect = screenRect (x, y)
        copy renderer fontTex (Just src_rect) (Just tgt_rect)
        ) pixels

    rendererRenderTarget renderer $= Nothing
    copy renderer winText Nothing Nothing
    present renderer

sendKey :: Word8 -> StateT Context IO ()
sendKey byte = do
    ctx <- get
    let apple = machine ctx
    let byte' = byte
    liftIO $ AppleI.sendKey apple byte'


getVBuffer :: StateT Context IO [Word8]
getVBuffer = do
    ctx <- get
    liftIO . AppleI.getVBuffer $ machine ctx

exitProgram :: StateT Context IO ()
exitProgram = modify (\ctx -> ctx{exitRequest = True})

reset :: StateT Context IO ()
reset = do
    ctx <- get
    let apple = machine ctx
    liftIO $ AppleI.reset apple

handleKeyboard :: SDL.KeyboardEventData -> StateT Context IO ()
handleKeyboard ke = do
    when (keyboardEventKeyMotion ke == Pressed) (do
            case keysymKeycode . keyboardEventKeysym $ ke of
                KeycodeReturn -> sendKey 0x8D
                KeycodeBackspace -> sendKey 0xDF
                KeycodeEscape -> sendKey 0x9B
                KeycodeF1 -> reset
                _ -> return ()
        )

handleText :: SDL.TextInputEventData -> StateT Context IO ()
handleText te = do
    let text = textInputEventText te
    let akey = appleKey text
    forM_ akey sendKey

handleEvents :: SDL.Event -> StateT Context IO ()
handleEvents e = do
    case SDL.eventPayload e of
        SDL.KeyboardEvent x -> handleKeyboard x
        SDL.TextInputEvent x -> handleText x
        SDL.QuitEvent -> exitProgram
        _ -> return ()
   
control :: StateT Context IO ()
control = do
    events <- pollEvents
    mapM_ handleEvents events

tick :: StateT Context IO ()
tick = do
    ctx <- get
    liftIO $ AppleI.tickN 960 (machine ctx)


fromChar :: Char -> Word8
fromChar = fromIntegral . fromEnum 

appleKey :: Text -> Maybe Word8
appleKey "@"               = Just ( fromChar '@' .|. 0x80 )
appleKey "A"               = Just ( fromChar 'A' .|. 0x80 )
appleKey "B"               = Just ( fromChar 'B' .|. 0x80 )
appleKey "C"               = Just ( fromChar 'C' .|. 0x80 )
appleKey "D"               = Just ( fromChar 'D' .|. 0x80 )
appleKey "E"               = Just ( fromChar 'E' .|. 0x80 )
appleKey "F"               = Just ( fromChar 'F' .|. 0x80 )
appleKey "G"               = Just ( fromChar 'G' .|. 0x80 )
appleKey "H"               = Just ( fromChar 'H' .|. 0x80 )
appleKey "I"               = Just ( fromChar 'I' .|. 0x80 )
appleKey "J"               = Just ( fromChar 'J' .|. 0x80 )
appleKey "K"               = Just ( fromChar 'K' .|. 0x80 )
appleKey "L"               = Just ( fromChar 'L' .|. 0x80 )
appleKey "M"               = Just ( fromChar 'M' .|. 0x80 )
appleKey "N"               = Just ( fromChar 'N' .|. 0x80 )
appleKey "O"               = Just ( fromChar 'O' .|. 0x80 )
appleKey "P"               = Just ( fromChar 'P' .|. 0x80 )
appleKey "Q"               = Just ( fromChar 'Q' .|. 0x80 )
appleKey "R"               = Just ( fromChar 'R' .|. 0x80 )
appleKey "S"               = Just ( fromChar 'S' .|. 0x80 )
appleKey "T"               = Just ( fromChar 'T' .|. 0x80 )
appleKey "U"               = Just ( fromChar 'U' .|. 0x80 )
appleKey "V"               = Just ( fromChar 'V' .|. 0x80 )
appleKey "W"               = Just ( fromChar 'W' .|. 0x80 )
appleKey "X"               = Just ( fromChar 'X' .|. 0x80 )
appleKey "Y"               = Just ( fromChar 'Y' .|. 0x80 )
appleKey "Z"               = Just ( fromChar 'Z' .|. 0x80 )
appleKey "a"               = Just ( fromChar 'A' .|. 0x80 )
appleKey "b"               = Just ( fromChar 'B' .|. 0x80 )
appleKey "c"               = Just ( fromChar 'C' .|. 0x80 )
appleKey "d"               = Just ( fromChar 'D' .|. 0x80 )
appleKey "e"               = Just ( fromChar 'E' .|. 0x80 )
appleKey "f"               = Just ( fromChar 'F' .|. 0x80 )
appleKey "g"               = Just ( fromChar 'G' .|. 0x80 )
appleKey "h"               = Just ( fromChar 'H' .|. 0x80 )
appleKey "i"               = Just ( fromChar 'I' .|. 0x80 )
appleKey "j"               = Just ( fromChar 'J' .|. 0x80 )
appleKey "k"               = Just ( fromChar 'K' .|. 0x80 )
appleKey "l"               = Just ( fromChar 'L' .|. 0x80 )
appleKey "m"               = Just ( fromChar 'M' .|. 0x80 )
appleKey "n"               = Just ( fromChar 'N' .|. 0x80 )
appleKey "o"               = Just ( fromChar 'O' .|. 0x80 )
appleKey "p"               = Just ( fromChar 'P' .|. 0x80 )
appleKey "q"               = Just ( fromChar 'Q' .|. 0x80 )
appleKey "r"               = Just ( fromChar 'R' .|. 0x80 )
appleKey "s"               = Just ( fromChar 'S' .|. 0x80 )
appleKey "t"               = Just ( fromChar 'T' .|. 0x80 )
appleKey "u"               = Just ( fromChar 'U' .|. 0x80 )
appleKey "v"               = Just ( fromChar 'V' .|. 0x80 )
appleKey "w"               = Just ( fromChar 'W' .|. 0x80 )
appleKey "x"               = Just ( fromChar 'X' .|. 0x80 )
appleKey "y"               = Just ( fromChar 'Y' .|. 0x80 )
appleKey "z"               = Just ( fromChar 'Z' .|. 0x80 )
appleKey "["               = Just ( fromChar '[' .|. 0x80 )
appleKey "\\"              = Just ( fromChar '\\' .|. 0x80 )
appleKey "]"               = Just ( fromChar ']' .|. 0x80 )
appleKey "^"               = Just ( fromChar '^' .|. 0x80 )
appleKey "_"               = Just ( fromChar '_' .|. 0x80 )
appleKey " "               = Just ( fromChar ' ' .|. 0x80 )
appleKey "!"               = Just ( fromChar '!' .|. 0x80 )
appleKey "\""              = Just ( fromChar '\"' .|. 0x80 )
appleKey "#"               = Just ( fromChar '#' .|. 0x80 )
appleKey "$"               = Just ( fromChar '$' .|. 0x80 )
appleKey "%"               = Just ( fromChar '%' .|. 0x80 )
appleKey "&"               = Just ( fromChar '&' .|. 0x80 )
appleKey "'"               = Just ( fromChar '\'' .|. 0x80 )
appleKey "("               = Just ( fromChar '(' .|. 0x80 )
appleKey ")"               = Just ( fromChar ')' .|. 0x80 )
appleKey "*"               = Just ( fromChar '*' .|. 0x80 )
appleKey "+"               = Just ( fromChar '+' .|. 0x80 )
appleKey ","               = Just ( fromChar ',' .|. 0x80 )
appleKey "-"               = Just ( fromChar '-' .|. 0x80 )
appleKey "."               = Just ( fromChar '.' .|. 0x80 )
appleKey "/"               = Just ( fromChar '/' .|. 0x80 )
appleKey "0"               = Just ( fromChar '0' .|. 0x80 )
appleKey "1"               = Just ( fromChar '1' .|. 0x80 )
appleKey "2"               = Just ( fromChar '2' .|. 0x80 )
appleKey "3"               = Just ( fromChar '3' .|. 0x80 )
appleKey "4"               = Just ( fromChar '4' .|. 0x80 )
appleKey "5"               = Just ( fromChar '5' .|. 0x80 )
appleKey "6"               = Just ( fromChar '6' .|. 0x80 )
appleKey "7"               = Just ( fromChar '7' .|. 0x80 )
appleKey "8"               = Just ( fromChar '8' .|. 0x80 )
appleKey "9"               = Just ( fromChar '9' .|. 0x80 )
appleKey ":"               = Just ( fromChar ':' .|. 0x80 )
appleKey ";"               = Just ( fromChar ';' .|. 0x80 )
appleKey "<"               = Just ( fromChar '<' .|. 0x80 )
appleKey "="               = Just ( fromChar '=' .|. 0x80 )
appleKey ">"               = Just ( fromChar '>' .|. 0x80 )
appleKey "?"               = Just ( fromChar '?' .|. 0x80 )
appleKey _ = Nothing
