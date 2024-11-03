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
    ,   dPalette :: Memory
    }

initialize :: Memory -> IO Display
initialize ram = do
    let displayMode = MODE_TEXT
    let page = PAGE_PRIMARY
    let graphicsMode = GRAPHICS_LORES
    let layout = LAYOUT_PURE
    cBank <- CB.characterBank
    palette <- Blocks.loadPalette
    return $ Display { dRAM = ram
                     , dDisplayMode = displayMode
                     , dPage = page
                     , dGraphicsMode = graphicsMode
                     , dLayout = layout
                     , dCBank = cBank 
                     , dPalette = palette }


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

charDemo :: Display -> Ptr () -> IO ()
charDemo display rawBuffer = do
    mapM_ (\y -> do
        mapM_ (\x -> do
            let char = fromIntegral $ mod (x + y * 40) 0xFF
            drawChar display (x, y) char rawBuffer
            ) [0..39]
        ) [0..23]

colorDemo :: Display -> Ptr () -> IO ()
colorDemo display rawBuffer = do
    mapM_ (\y -> do
        mapM_ (\x -> do
            let col = fromIntegral $ mod (x + y * 40) 0xFF
            drawBlock display (x, y) col rawBuffer
            ) [0..39]
        ) [0..23]

pageBase :: Page -> Int
pageBase PAGE_PRIMARY = 0x400
pageBase PAGE_SECONDARY = 0x800

-- Ptr should be a ptr holding 280 x 192 x 3 bytes of data, representing (R, G, B) values of a display of 280 x 192 pixels.
updateVBuffer :: Display -> Ptr () -> IO ()
updateVBuffer display rawBuffer = do
    let graphicsMode  = dGraphicsMode display
    case graphicsMode of
        GRAPHICS_HIRES -> hiResRender display rawBuffer 
        GRAPHICS_LORES -> loResRender display rawBuffer

loResRender :: Display -> Ptr () -> IO ()
loResRender display rawBuffer = do
    let layout = dLayout display
    let displayMode = dDisplayMode display
    let page = dPage display
    let upperRenderer = case layout of
            LAYOUT_MIXED -> drawBlock
            LAYOUT_PURE -> case displayMode of
                MODE_GRAPHICS -> drawBlock
                MODE_TEXT -> drawChar
    let lowerRenderer = case layout of
            LAYOUT_MIXED -> drawChar
            LAYOUT_PURE -> case displayMode of
                MODE_GRAPHICS -> drawBlock
                MODE_TEXT -> drawChar

    let baseAddress = fromIntegral $ pageBase page :: Int

    -- Render upper segment
    mapM_ (\y -> do
        mapM_ (\x -> do
            let offset = (loResMap !! y) !! x
            let address = fromIntegral $ baseAddress + offset
            byte <- readByte (dRAM display) address
            upperRenderer display (x, y) byte rawBuffer
            ) [0..39]
        ) [0..19]
    -- Render lower segment
    mapM_ (\y -> do
        mapM_ (\x -> do
            let offset = (loResMap !! y) !! x
            let address = fromIntegral $ baseAddress + offset
            byte <- readByte (dRAM display) address
            lowerRenderer display (x, y) byte rawBuffer
            ) [0..39]
        ) [20..23]

hiResRender :: Display -> Ptr () -> IO ()
hiResRender = undefined
    

-- Lo-Res drawing calls

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
    bytes <- Blocks.getBlock (dPalette display) colByte
    mapM_ (\h -> do
        mapM_ (\w -> do
            let offset = 3 * (280 * h + w)
            let (r, g, b) = (bytes !! h) !! w :: Blocks.RGB
            poke (plusPtr vBuffer (start + offset + 0)) r
            poke (plusPtr vBuffer (start + offset + 1)) g
            poke (plusPtr vBuffer (start + offset + 2)) b
            ) [0..6]
        ) [0..7]

loResMap :: [[Int]]
loResMap = [[base + offset | offset <- [0..39]] | base <- lineAddr ] where
    lineAddr = [ 0x000 , 0x080 , 0x100 
               , 0x180 , 0x200 , 0x280 
               , 0x300 , 0x380 , 0x28 
               , 0x0A8 , 0x128 , 0x1A8 
               , 0x228 , 0x2A8 , 0x328 
               , 0x3A8 , 0x050 , 0x0D0 
               , 0x150 , 0x1D0 , 0x250
               , 0x2D0 , 0x350 , 0x3D0 ]
