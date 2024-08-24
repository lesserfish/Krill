{-# LANGUAGE OverloadedStrings #-}

module Frontend.Internal where

import qualified AppleI.Terminal as Term

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

initSDL :: [Word8] -> FilePath -> IO Context
initSDL romData fontFP = do
    initializeAll
    let winsize = 2 * V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    let texsize = V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    window <- createWindow "Apple I" defaultWindow{windowInitialSize =  winsize}
    renderer <- createRenderer window (-1) defaultRenderer{ rendererTargetTexture = True }
    fontSurf <- loadBMP fontFP
    fontText <- createTextureFromSurface renderer fontSurf
    winText <- createTexture renderer RGBA8888 TextureAccessTarget texsize
    m <- AppleI.new romData
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

handleKeyboard :: SDL.KeyboardEventData -> StateT Context IO ()
handleKeyboard ke = do
    when (keyboardEventKeyMotion ke == Pressed) (do
            case keysymKeycode . keyboardEventKeysym $ ke of
                KeycodeReturn -> sendKey 0x8D
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

debug :: StateT Context IO ()
debug = do
    ctx <- get
    liftIO $ AppleI.debug (machine ctx)

tick :: StateT Context IO ()
tick = do
    ctx <- get
    liftIO $ AppleI.tickN 960 (machine ctx)
    debug

appleKey :: Text -> Maybe Word8
appleKey "@"               = Just 0x80
appleKey "A"               = Just 0x81
appleKey "B"               = Just 0x82
appleKey "C"               = Just 0x83
appleKey "D"               = Just 0x84
appleKey "E"               = Just 0x85
appleKey "F"               = Just 0x86
appleKey "G"               = Just 0x87
appleKey "H"               = Just 0x88
appleKey "I"               = Just 0x89
appleKey "J"               = Just 0x8A
appleKey "K"               = Just 0x8B
appleKey "L"               = Just 0x8C
appleKey "M"               = Just 0x8D
appleKey "N"               = Just 0x8E
appleKey "O"               = Just 0x8F
appleKey "P"               = Just 0x90
appleKey "Q"               = Just 0x91
appleKey "R"               = Just 0x92
appleKey "S"               = Just 0x93
appleKey "T"               = Just 0x94
appleKey "U"               = Just 0x95
appleKey "V"               = Just 0x96
appleKey "W"               = Just 0x97
appleKey "X"               = Just 0x98
appleKey "Y"               = Just 0x99
appleKey "Z"               = Just 0x9A
appleKey "a"               = Just 0x81
appleKey "b"               = Just 0x82
appleKey "c"               = Just 0x83
appleKey "d"               = Just 0x84
appleKey "e"               = Just 0x85
appleKey "f"               = Just 0x86
appleKey "g"               = Just 0x87
appleKey "h"               = Just 0x88
appleKey "i"               = Just 0x89
appleKey "j"               = Just 0x8A
appleKey "k"               = Just 0x8B
appleKey "l"               = Just 0x8C
appleKey "m"               = Just 0x8D
appleKey "n"               = Just 0x8E
appleKey "o"               = Just 0x8F
appleKey "p"               = Just 0x90
appleKey "q"               = Just 0x91
appleKey "r"               = Just 0x92
appleKey "s"               = Just 0x93
appleKey "t"               = Just 0x94
appleKey "u"               = Just 0x95
appleKey "v"               = Just 0x96
appleKey "w"               = Just 0x97
appleKey "x"               = Just 0x98
appleKey "y"               = Just 0x99
appleKey "z"               = Just 0x9A
appleKey "["               = Just 0x9B
appleKey "\\"              = Just 0x9C
appleKey "]"               = Just 0x9D
appleKey "^"               = Just 0x9E
appleKey "_"               = Just 0x9F
appleKey " "               = Just 0xA0
appleKey "!"               = Just 0xA1
appleKey "\""              = Just 0xA2
appleKey "#"               = Just 0xA3
appleKey "$"               = Just 0xA4
appleKey "%"               = Just 0xA5
appleKey "&"               = Just 0xA6
appleKey "'"               = Just 0xA7
appleKey "("               = Just 0xA8
appleKey ")"               = Just 0xA9
appleKey "*"               = Just 0xAA
appleKey "+"               = Just 0xAB
appleKey ","               = Just 0xAC
appleKey "-"               = Just 0xAD
appleKey "."               = Just 0xAE
appleKey "/"               = Just 0xAF
appleKey "0"               = Just 0xB0
appleKey "1"               = Just 0xB1
appleKey "2"               = Just 0xB2
appleKey "3"               = Just 0xB3
appleKey "4"               = Just 0xB4
appleKey "5"               = Just 0xB5
appleKey "6"               = Just 0xB6
appleKey "7"               = Just 0xB7
appleKey "8"               = Just 0xB8
appleKey "9"               = Just 0xB9
appleKey ":"               = Just 0xBA
appleKey ";"               = Just 0xBB
appleKey "<"               = Just 0xBC
appleKey "="               = Just 0xBD
appleKey ">"               = Just 0xBE
appleKey "?"               = Just 0xBF
appleKey _ = Nothing
