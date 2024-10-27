module AppleII.Display (
    Display(..)
,   initialize
,   cpuRead
,   cpuWrite
,   updateVBuffer
,   tick
) where

import qualified AppleII.Display.Char as CB
import AppleII.Memory
import Control.Monad.State
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import System.Random (randomIO)

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
    ,   dCBank :: CB.CharacterBank
    }

initialize :: Memory -> IO Display
initialize ram = do
    let displayMode = MODE_TEXT
    let page = PAGE_PRIMARY
    let graphicsMode = GRAPHICS_LORES
    let layout = LAYOUT_PURE
    cBank <- CB.characterBank
    return $ Display { dRAM = ram
                     , dDisplayMode = displayMode
                     , dPage = page
                     , dGraphicsMode = graphicsMode
                     , dLayout = layout
                     , dCBank = cBank }


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

updateCBank :: StateT Display IO ()
updateCBank = do
    display <- get
    put display { dCBank = CB.tick ( dCBank display )}


tick' :: StateT Display IO ()
tick' = do
    updateCBank

tick :: Display -> IO Display
tick = execStateT tick' 

-- Rendering methods

-- Ptr should be a ptr holding 280 x 192 x 3 bytes of data, representing (R, G, B) values of a display of 280 x 192 pixels.
updateVBuffer :: Display -> Ptr () -> IO ()
updateVBuffer display rawBuffer = do
    let vBuffer = castPtr rawBuffer :: Ptr Word8
    mapM_ (\idx -> do
        byte <- randomIO :: IO Word8
        poke (plusPtr vBuffer idx) byte
        ) [0..(280 * 192 * 3)]

