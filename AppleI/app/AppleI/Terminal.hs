module AppleI.Terminal where

import Data.Word
import Control.Monad.State
import qualified AppleI.Carousel as C
import qualified AppleI.Memory as M

-- There is very little documentation on the APPLE I terminal.
-- You are essentially forced to read the schematics, or learn from people who did.
-- This is an abstract, overview kind of emulation of the APPLE I terminal.
-- It aims to vaguely resemble the original, much like a Pollock painting vaguely resembles a cat.
-- I claim no accuracy whatsoever. Here are some sources you may enjoy:
--
-- https://retrocomputing.stackexchange.com/questions/13228/how-did-the-apple-1-video-circuit-work
-- http://www.righto.com/2022/04/inside-apple-1s-shift-register-memory.html
-- https://www.sbprojects.net/projects/apple1/terminal.php
-- https://www.youtube.com/watch?v=36NgkpctW6k
-- https://www.youtube.com/watch?v=BHeUbAVllJo
-- https://github.com/The8BitEnthusiast/apple-1-video-terminal-on-fpga
--
-- Is there even any point in simulating this accurately. Even what I did is probably too much.
--

data TState = NORMAL

data Terminal = Terminal {
    dCursorChar         :: Word8,
    dChar               :: Word8,
    dVideoShiftRegister :: C.Carousel,
    dLineRegister       :: C.Carousel,
    dVideoBuffer        :: M.Memory,
    dCurrentLine        :: Int,
    dCurrentCursor      :: Int,
    dState              :: TState,
    dCounter            :: Int
}

new :: IO Terminal
new = do
    vb <- M.fromList M.ReadAccess (replicate (24 * 40) 0xA0)
    vsr <- C.new 960
    lr <- C.new 40
    return $ Terminal { dCursorChar = 0x00
                      , dChar = 0x00
                      , dVideoShiftRegister = vsr
                      , dLineRegister = lr
                      , dVideoBuffer = vb
                      , dCurrentLine = 23
                      , dCurrentCursor = 0
                      , dState = NORMAL
                      , dCounter = 0}

updateChar :: Word8 -> StateT Terminal IO ()
updateChar char = modify (\term -> term {dChar = char})

offsetCursor :: Int -> StateT Terminal IO ()
offsetCursor offset = modify (\term -> term {dCurrentCursor = mod (dCurrentCursor term + offset) 960})

offsetLine :: Int -> StateT Terminal IO ()
offsetLine offset = modify (\term -> term {dCurrentLine = mod (dCurrentLine term + offset) 24})

updateLine :: Int -> StateT Terminal IO ()
updateLine line = modify (\term -> term {dCurrentLine = line})

sendChar :: Word8 -> Terminal -> Terminal
sendChar char term = term' where
    currentChar = dChar term
    term' = if currentChar > 0x7F then term else term {dChar = char}

getVBuffer :: Terminal -> IO [Word8]
getVBuffer term = do
    M.toList . dVideoBuffer $ term

shiftVSR :: StateT Terminal IO Word8
shiftVSR = do
    term <- get
    let vsr = dVideoShiftRegister term
    (byte, vsr') <- liftIO $ runStateT C.shift vsr
    put term {dVideoShiftRegister = vsr'}
    return byte

shiftLR :: StateT Terminal IO Word8
shiftLR = do
    term <- get
    let lr = dLineRegister term
    (byte, lr') <- liftIO $ runStateT C.shift lr
    put term {dLineRegister = lr'}
    return byte

pushVSR :: Word8 -> StateT Terminal IO Word8
pushVSR inbyte = do
    term <- get
    let vsr = dVideoShiftRegister term
    (outbyte, vsr') <- liftIO $ runStateT (C.push inbyte) vsr
    put term {dVideoShiftRegister = vsr'}
    return outbyte

pushLR :: Word8 -> StateT Terminal IO Word8
pushLR inbyte = do
    term <- get
    let lr = dLineRegister term
    (outbyte, lr') <- liftIO $ runStateT (C.push inbyte) lr
    put term {dLineRegister = lr'}
    return outbyte

handleKey :: Word8 -> StateT Terminal IO ()
handleKey char = do
    pushChar char
    updateChar 0x00
    
pushChar :: Word8 -> StateT Terminal IO ()
pushChar 0xB1 = do
    offsetCursor 40
pushChar 0xB2 = do
    offsetLine (-1)
    offsetCursor 40

pushChar char = do
    byte <- pushVSR char
    _ <- pushLR byte
    offsetCursor 2


pushLine :: [Word8] -> StateT Terminal IO ()
pushLine linebuff = do
    videobuff <- gets dVideoBuffer
    line <- gets dCurrentLine
    let address = [line * 40 + x | x <- [0..40]]
    let linedata = zip address linebuff
    forM_ linedata (\(addr, char) -> do
            let addr' = fromIntegral addr
            liftIO $ M.writeByte videobuff addr' char
            return ()
        )

updateCarousel :: StateT Terminal IO ()
updateCarousel = do
    char <- gets dChar
    cursor <- gets dCurrentCursor
    if cursor == 0 && char > 0x7F then handleKey char
        else do
            byte <- shiftVSR
            _ <- pushLR byte
            offsetCursor 1
            return ()


updateDisplay :: StateT Terminal IO ()
updateDisplay = do
    lr <- gets dLineRegister
    let shiftPos = C.cPosition lr
    when (shiftPos == 0) (do
        linebuff <- liftIO $ C.toList lr
        pushLine linebuff
        offsetLine (-1))

tick' :: StateT Terminal IO ()
tick' = do
    updateCarousel
    updateDisplay

tick :: Terminal -> IO Terminal
tick = execStateT tick'

tickN :: Int -> Terminal -> IO Terminal
tickN n term = do
    let action = replicateM_ n tick'
    execStateT action term
