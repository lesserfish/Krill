{-# LANGUAGE OverloadedStrings #-}

module Frontend.Internal where

import qualified AppleI.Terminal as Term

import Text.Printf
import Data.Text (Text)
import Data.Foldable (forM_)
import SDL hiding (get)
import Foreign.C.Types (CInt)
import Control.Monad.State
import Data.Word
import Data.Bits
import qualified AppleI.Terminal as Terminal

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
    terminal      :: Terminal.Terminal
}

initSDL :: FilePath -> IO Context
initSDL filepath = do
    initializeAll
    let winsize = 2 * V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    let texsize = V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    window <- createWindow "Apple I" defaultWindow{windowInitialSize =  winsize}
    renderer <- createRenderer window (-1) defaultRenderer{ rendererTargetTexture = True }
    fontSurf <- loadBMP filepath
    fontText <- createTextureFromSurface renderer fontSurf
    winText <- createTexture renderer RGBA8888 TextureAccessTarget texsize
    term <- Term.new
    return Context {  sdlWindow = window
                    , sdlRenderer = renderer
                    , fontTexture = fontText
                    , windowTexture = winText
                    , exitRequest = False
                    , terminal = term}

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
    let byte' = byte + 0x80
    let term' = Term.sendChar byte' (terminal ctx)
    put ctx{terminal = term'}


getVBuffer :: StateT Context IO [Word8]
getVBuffer = do
    ctx <- get
    liftIO . Term.getVBuffer $ terminal ctx

exitProgram :: StateT Context IO ()
exitProgram = modify (\ctx -> ctx{exitRequest = True})

handleKeyboard :: SDL.KeyboardEventData -> StateT Context IO ()
handleKeyboard ke = do
    when (keyboardEventKeyMotion ke == Pressed) (do
            case keysymKeycode . keyboardEventKeysym $ ke of
                KeycodeReturn -> sendKey (0xFF - 0x80)
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
    term' <- liftIO $ Term.tickN 960 (terminal ctx)
    put ctx{terminal = term'}

appleKey :: Text -> Maybe Word8
appleKey "@"               = Just 0x00
appleKey "A"               = Just 0x01
appleKey "B"               = Just 0x02
appleKey "C"               = Just 0x03
appleKey "D"               = Just 0x04
appleKey "E"               = Just 0x05
appleKey "F"               = Just 0x06
appleKey "G"               = Just 0x07
appleKey "H"               = Just 0x08
appleKey "I"               = Just 0x09
appleKey "J"               = Just 0x0A
appleKey "K"               = Just 0x0B
appleKey "L"               = Just 0x0C
appleKey "M"               = Just 0x0D
appleKey "N"               = Just 0x0E
appleKey "O"               = Just 0x0F
appleKey "P"               = Just 0x10
appleKey "Q"               = Just 0x11
appleKey "R"               = Just 0x12
appleKey "S"               = Just 0x13
appleKey "T"               = Just 0x14
appleKey "U"               = Just 0x15
appleKey "V"               = Just 0x16
appleKey "W"               = Just 0x17
appleKey "X"               = Just 0x18
appleKey "Y"               = Just 0x19
appleKey "Z"               = Just 0x1A
appleKey "a"               = Just 0x01
appleKey "b"               = Just 0x02
appleKey "c"               = Just 0x03
appleKey "d"               = Just 0x04
appleKey "e"               = Just 0x05
appleKey "f"               = Just 0x06
appleKey "g"               = Just 0x07
appleKey "h"               = Just 0x08
appleKey "i"               = Just 0x09
appleKey "j"               = Just 0x0A
appleKey "k"               = Just 0x0B
appleKey "l"               = Just 0x0C
appleKey "m"               = Just 0x0D
appleKey "n"               = Just 0x0E
appleKey "o"               = Just 0x0F
appleKey "p"               = Just 0x10
appleKey "q"               = Just 0x11
appleKey "r"               = Just 0x12
appleKey "s"               = Just 0x13
appleKey "t"               = Just 0x14
appleKey "u"               = Just 0x15
appleKey "v"               = Just 0x16
appleKey "w"               = Just 0x17
appleKey "x"               = Just 0x18
appleKey "y"               = Just 0x19
appleKey "z"               = Just 0x1A
appleKey "["               = Just 0x1B
appleKey "\\"              = Just 0x1C
appleKey "]"               = Just 0x1D
appleKey "^"               = Just 0x1E
appleKey "_"               = Just 0x1F
appleKey " "               = Just 0x20
appleKey "!"               = Just 0x21
appleKey "\""              = Just 0x22
appleKey "#"               = Just 0x23
appleKey "$"               = Just 0x24
appleKey "%"               = Just 0x25
appleKey "&"               = Just 0x26
appleKey "'"               = Just 0x27
appleKey "("               = Just 0x28
appleKey ")"               = Just 0x29
appleKey "*"               = Just 0x2A
appleKey "+"               = Just 0x2B
appleKey ","               = Just 0x2C
appleKey "-"               = Just 0x2D
appleKey "."               = Just 0x2E
appleKey "/"               = Just 0x2F
appleKey "0"               = Just 0x30
appleKey "1"               = Just 0x31
appleKey "2"               = Just 0x32
appleKey "3"               = Just 0x33
appleKey "4"               = Just 0x34
appleKey "5"               = Just 0x35
appleKey "6"               = Just 0x36
appleKey "7"               = Just 0x37
appleKey "8"               = Just 0x38
appleKey "9"               = Just 0x39
appleKey ":"               = Just 0x3A
appleKey ";"               = Just 0x3B
appleKey "<"               = Just 0x3C
appleKey "="               = Just 0x3D
appleKey ">"               = Just 0x3E
appleKey "?"               = Just 0x3F
appleKey _ = Nothing
