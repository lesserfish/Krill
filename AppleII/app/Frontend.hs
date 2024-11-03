{-# LANGUAGE OverloadedStrings #-}

module Frontend where

import SDL hiding (get)
import Foreign.C.Types (CInt)
import Control.Monad.State
import Control.Monad
import Data.Word
import qualified AppleII.Bus as Apple
import Frontend.KeyASCII 

_CHAR_WIDTH :: CInt 
_CHAR_WIDTH = 16

_CHAR_HEIGHT :: CInt 
_CHAR_HEIGHT = 16

data Context = Context {
    apple         :: Apple.AppleII,
    sdlWindow     :: Window,
    sdlRenderer   :: Renderer,
    videoTexture   :: Texture,
    exitRequest   :: Bool}

initialize :: IO Context
initialize = do
    initializeAll
    let winsize = 4 * V2 280 192
    let buffsize = V2 280 192
    window <- createWindow "Apple II" defaultWindow{windowInitialSize =  winsize}
    renderer <- createRenderer window (-1) defaultRenderer{ rendererTargetTexture = True }
    vTexture <- createTexture renderer RGB24 TextureAccessStreaming buffsize
    appl <- Apple.new [] []
    return Context {  sdlWindow = window
                    , sdlRenderer = renderer
                    , videoTexture = vTexture
                    , exitRequest = False
                    , apple = appl
                   }

screenRect :: (Int, Int) -> Rectangle CInt
screenRect (a, b) = Rectangle (P $ V2 x y) (V2 _CHAR_WIDTH _CHAR_HEIGHT) where
    x = fromIntegral a * _CHAR_WIDTH
    y = fromIntegral b * _CHAR_HEIGHT

updateVBuffer :: StateT Context IO ()
updateVBuffer = do
    appl <- gets apple
    vTexture <- gets videoTexture
    (rawBuffer, _) <- lockTexture vTexture Nothing
    liftIO $ Apple.updateVBuffer appl rawBuffer
    unlockTexture vTexture

renderVBuffer :: StateT Context IO ()
renderVBuffer = do
    renderer <- gets sdlRenderer
    vTexture <- gets videoTexture
    rendererRenderTarget renderer $= Nothing
    copy renderer vTexture Nothing Nothing
    present renderer

sendKey :: Word8 -> StateT Context IO ()
sendKey _ = do
    return ()

specialKey :: Keycode -> StateT Context IO ()
specialKey KeycodeF12 = reset
specialKey _ = return ()

exitProgram :: StateT Context IO ()
exitProgram = modify (\ctx -> ctx{exitRequest = True})

reset :: StateT Context IO ()
reset = do
    return ()

handleKeyboard :: SDL.KeyboardEventData -> StateT Context IO ()
handleKeyboard ke = do
    when (keyboardEventKeyMotion ke == Pressed) (do
            modState <- getModState
            let ctrl = keyModifierLeftCtrl modState || keyModifierRightCtrl modState
            let shift = keyModifierLeftShift modState || keyModifierRightShift modState
            let kmod = keyMod (ctrl, shift)
            let key = keysymKeycode . keyboardEventKeysym $ ke
            let maybeByte = keyASCII kmod key
            case maybeByte of
                Nothing -> specialKey key
                Just byte -> sendKey (fromIntegral byte)
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

tickApple :: StateT Context IO ()
tickApple = do
    appl <- gets apple
    liftIO $ Apple.tickN 200 appl

tick :: StateT Context IO ()
tick = do
    tickApple
    updateVBuffer
    return ()

