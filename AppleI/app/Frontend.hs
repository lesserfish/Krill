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

initialize :: [Word8] -> [Word8] -> [Word8] -> FilePath -> IO Context
initialize biosData ciData cassetteData fontFP = do
    initializeAll
    let winsize = 2 * V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    let texsize = V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    window <- createWindow "Apple I" defaultWindow{windowInitialSize =  winsize}
    renderer <- createRenderer window (-1) defaultRenderer{ rendererTargetTexture = True }
    fontSurf <- loadBMP fontFP
    fontText <- createTextureFromSurface renderer fontSurf
    winText <- createTexture renderer RGBA8888 TextureAccessTarget texsize
    m <- AppleI.new biosData ciData cassetteData
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

appleKey :: Text -> Maybe Word8
appleKey "@"               = Just 0x80
appleKey "A"               = Just 0xC1
appleKey "B"               = Just 0xC2
appleKey "C"               = Just 0xC3
appleKey "D"               = Just 0xC4
appleKey "E"               = Just 0xC5
appleKey "F"               = Just 0xC6
appleKey "G"               = Just 0xC7
appleKey "H"               = Just 0xC8
appleKey "I"               = Just 0xC9
appleKey "J"               = Just 0xCA
appleKey "K"               = Just 0xCB
appleKey "L"               = Just 0xCC
appleKey "M"               = Just 0xCD
appleKey "N"               = Just 0xCE
appleKey "O"               = Just 0xCF
appleKey "P"               = Just 0xD0
appleKey "Q"               = Just 0xD1
appleKey "R"               = Just 0xD2
appleKey "S"               = Just 0xD3
appleKey "T"               = Just 0xD4
appleKey "U"               = Just 0xD5
appleKey "V"               = Just 0xD6
appleKey "W"               = Just 0xD7
appleKey "X"               = Just 0xD8
appleKey "Y"               = Just 0xD9
appleKey "Z"               = Just 0xDA
appleKey "a"               = Just 0xC1
appleKey "b"               = Just 0xC2
appleKey "c"               = Just 0xC3
appleKey "d"               = Just 0xC4
appleKey "e"               = Just 0xC5
appleKey "f"               = Just 0xC6
appleKey "g"               = Just 0xC7
appleKey "h"               = Just 0xC8
appleKey "i"               = Just 0xC9
appleKey "j"               = Just 0xCA
appleKey "k"               = Just 0xCB
appleKey "l"               = Just 0xCC
appleKey "m"               = Just 0xCD
appleKey "n"               = Just 0xCE
appleKey "o"               = Just 0xCF
appleKey "p"               = Just 0xD0
appleKey "q"               = Just 0xD1
appleKey "r"               = Just 0xD2
appleKey "s"               = Just 0xD3
appleKey "t"               = Just 0xD4
appleKey "u"               = Just 0xD5
appleKey "v"               = Just 0xD6
appleKey "w"               = Just 0xD7
appleKey "x"               = Just 0xD8
appleKey "y"               = Just 0xD9
appleKey "z"               = Just 0xDA
appleKey "["               = Just 0x9B
appleKey "\\"              = Just 0xDC
appleKey "]"               = Just 0xDD
appleKey "^"               = Just 0xDE
appleKey "_"               = Just 0xDF
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
