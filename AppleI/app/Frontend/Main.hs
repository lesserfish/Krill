module Frontend.Main where
import Frontend.Internal

import qualified Data.ByteString as B
import Data.Word
import Control.Monad.State

loadBinary :: FilePath -> IO [Word8]
loadBinary path = B.unpack <$> B.readFile path

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
    let fontFP = "/home/lesserfish/Drive/Code/Krill/AppleI/Assets/font.bmp"
    romData <- loadBinary "/home/lesserfish/Drive/Code/Krill/AppleI/Basic/wozmon.bin"
    ctx <- initSDL romData fontFP
    _ <- execStateT loop ctx
    return ()
