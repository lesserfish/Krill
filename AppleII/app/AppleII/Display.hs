module AppleII.Display (
    Display(..)
,   cpuRead
,   cpuWrite
,   renderBuffer
) where

import AppleII.Memory
import Control.Monad.State
import Foreign.Ptr
import Data.Word

data GraphicsMode = GRAPHICS_LORES
                  | GRAPHICS_HIRES

data DisplayMode = MODE_TEXT
                 | MODE_GRAPHICS

data Layout =  LAYOUT_MIXED
             | LAYOUT_PURE

data Page = PAGE_PRIMARY
          | PAGE_SECONDARY


data Display = Display 
    {   dRAM :: Memory
    ,   dDisplayMode :: DisplayMode
    ,   dPage :: Page
    ,   dGraphicsMode :: GraphicsMode
    ,   dLayout :: Layout
    }

cpuRef :: Word16 -> StateT Display IO ()
cpuRef addr 
    | addr == 0xC050 =  -- Switch to Graphics mode
        modify (\d -> d{ dDisplayMode = MODE_GRAPHICS })
    | addr == 0xC051 =  -- Switch to Text mode
        modify (\d -> d{ dDisplayMode = MODE_TEXT })
    | addr == 0xC052 =  -- Display All Text / All Graphics
        modify (\d -> d{ dLayout = LAYOUT_PURE })
    | addr == 0xC053 = -- Mix Text / Graphics
        modify (\d -> d{ dLayout = LAYOUT_MIXED })
    | addr == 0xC054 = -- Display the primary page
        modify (\d -> d{ dPage = PAGE_PRIMARY })
    | addr == 0xC055 =  -- Display the secondary page
        modify (\d -> d{ dPage = PAGE_SECONDARY })
    | addr == 0xC056 = -- Display Lo-Res Graphics Mode
        modify (\d -> d{ dGraphicsMode = GRAPHICS_LORES })
    | addr == 0xC057 = -- Display Hi-Res Graphics Mode
        modify (\d -> d{ dGraphicsMode = GRAPHICS_HIRES })
    | otherwise = return () --


cpuRead :: Word16 -> Display -> IO (Word8, Display)
cpuRead addr = runStateT (cpuRef addr >> return 0)

cpuWrite :: Word16 -> Word8 -> Display -> IO Display
cpuWrite addr _ = execStateT (cpuRef addr )

-- In Text-Mode the display can render 40x24 text characters, each 7x8 pixels, for a total resolution of 280x192 pixels.
-- In Lores-Mode the display can render 40x48 color blocks, each 7x4 pixels, for a total resolution of 280x192 pixels.
-- In Hires-Mode the display can render 280x192 pixels.
renderBuffer' :: Ptr () -> StateT Display IO ()
renderBuffer' rawBuffer = do
    let byteBuffer = castPtr rawBuffer :: Ptr Word8
    return ()

renderBuffer :: Display -> Ptr () -> IO Display
renderBuffer display rawBuffer = execStateT (renderBuffer' rawBuffer) display
