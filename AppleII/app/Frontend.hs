{-# LANGUAGE OverloadedStrings #-}

module Frontend where

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
    windowTexture :: Texture,
    exitRequest   :: Bool}

initialize :: IO Context
initialize = do
    initializeAll
    let winsize = 2 * V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    let texsize = V2 (40 * _CHAR_WIDTH) (24 * _CHAR_HEIGHT)
    window <- createWindow "Apple II" defaultWindow{windowInitialSize =  winsize}
    renderer <- createRenderer window (-1) defaultRenderer{ rendererTargetTexture = True }
    winText <- createTexture renderer RGBA8888 TextureAccessTarget texsize
    return Context {  sdlWindow = window
                    , sdlRenderer = renderer
                    , windowTexture = winText
                    , exitRequest = False
                   }

screenRect :: (Int, Int) -> Rectangle CInt
screenRect (a, b) = Rectangle (P $ V2 x y) (V2 _CHAR_WIDTH _CHAR_HEIGHT) where
    x = fromIntegral a * _CHAR_WIDTH
    y = fromIntegral b * _CHAR_HEIGHT

getVBuffer :: StateT Context IO [Word8]
getVBuffer = do
    return []

renderVBuffer :: [Word8] -> StateT Context IO ()
renderVBuffer buffer = do
    renderer <- gets sdlRenderer
    winText <- gets windowTexture
    rendererRenderTarget renderer $= Nothing
    copy renderer winText Nothing Nothing
    present renderer

sendKey :: Word8 -> StateT Context IO ()
sendKey _ = do
    return ()

exitProgram :: StateT Context IO ()
exitProgram = modify (\ctx -> ctx{exitRequest = True})

reset :: StateT Context IO ()
reset = do
    return ()

handleKeyboard :: SDL.KeyboardEventData -> StateT Context IO ()
handleKeyboard ke = do
    when (keyboardEventKeyMotion ke == Pressed) (do
            case keysymKeycode . keyboardEventKeysym $ ke of
                _ -> return ()
        )

handleEvents :: SDL.Event -> StateT Context IO ()
handleEvents e = do
    case SDL.eventPayload e of
        SDL.KeyboardEvent x -> handleKeyboard x
        SDL.QuitEvent -> exitProgram
        _ -> return ()
   
control :: StateT Context IO ()
control = do
    events <- pollEvents
    mapM_ handleEvents events

tick :: StateT Context IO ()
tick = do
    ctx <- get
    return ()

