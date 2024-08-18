module Frontend.Main where
import Frontend.Internal

import Control.Monad.State

loop :: StateT Context IO ()
loop = do
    tick
    control
    exit <- gets exitRequest
    buffer <- getVBuffer
    renderVBuffer buffer
    unless exit loop

main :: IO ()
main = do
    ctx <- initSDL "/home/lesserfish/Drive/Code/Krill/AppleI/Assets/font.bmp"
    _ <- execStateT loop ctx
    return ()
