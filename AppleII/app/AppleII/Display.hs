module AppleII.Display (
    Display(..)
,   initialize
,   cpuRead
,   cpuWrite
,   updateVBuffer
,   tick
) where

import qualified AppleII.Display.Char as CB
import qualified AppleII.Display.Blocks as Blocks
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
    bank' <- liftIO $ CB.tick ( dCBank display )
    put display { dCBank = bank' }


tick' :: StateT Display IO ()
tick' = do
    updateCBank

tick :: Display -> IO Display
tick = execStateT tick' 

-- Rendering methods

-- Ptr should be a ptr holding 280 x 192 x 3 bytes of data, representing (R, G, B) values of a display of 280 x 192 pixels.
updateVBuffer1 :: Display -> Ptr () -> IO ()
updateVBuffer1 display rawBuffer = do
    mapM_ (\y -> do
        mapM_ (\x -> do
            let char = fromIntegral $ mod (x + y * 40) 0xFF
            drawChar display (x, y) char rawBuffer
            ) [0..39]
        ) [0..23]

updateVBuffer2 :: Display -> Ptr () -> IO ()
updateVBuffer2 display rawBuffer = do
    mapM_ (\y -> do
        mapM_ (\x -> do
            let col = fromIntegral $ mod (x + y * 40) 0xFF
            drawBlock display (x, y) col rawBuffer
            ) [0..39]
        ) [0..23]


updateVBuffer :: Display -> Ptr () -> IO ()
updateVBuffer = updateVBuffer2

drawChar :: Display -> (Int, Int) -> Word8 -> Ptr () -> IO ()
drawChar display (x, y) char rawBuffer = do
    let vBuffer = castPtr rawBuffer :: Ptr Word8
    let start = 3 * (y * 280 * 8  + x * 7)
    bytes <- CB.getGlyph (dCBank display) char
    mapM_ (\h -> do
        mapM_ (\w -> do
            let offset = 3 * (280 * h + w)
            let byte = (bytes !! h) !! w :: Word8
            poke (plusPtr vBuffer (start + offset + 0)) byte
            poke (plusPtr vBuffer (start + offset + 1)) byte
            poke (plusPtr vBuffer (start + offset + 2)) byte
            ) [0..6]
        ) [0..7]

drawBlock :: Display -> (Int, Int) -> Word8 -> Ptr () -> IO ()
drawBlock display (x, y) colByte rawBuffer = do
    let vBuffer = castPtr rawBuffer :: Ptr Word8
    let start = 3 * (y * 280 * 8  + x * 7)
    let bytes = Blocks.getBlock colByte
    mapM_ (\h -> do
        mapM_ (\w -> do
            let offset = 3 * (280 * h + w)
            let (r, g, b) = (bytes !! h) !! w :: Blocks.RGB
            poke (plusPtr vBuffer (start + offset + 0)) r
            poke (plusPtr vBuffer (start + offset + 1)) g
            poke (plusPtr vBuffer (start + offset + 2)) b
            ) [0..6]
        ) [0..7]
