module AppleI.Terminal (
    Terminal,
    reset,
    writeChar,
    readChar,
    new,
    getVBuffer,
    tick,
    tickN
) where

import Data.Bits (xor)
import Data.Word
import Control.Monad.State
import qualified AppleI.Terminal.Carousel as C
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

data Terminal = Terminal {
    dCursorChar         :: Word8,
    dChar               :: Word8,
    dVideoShiftRegister :: C.Carousel,
    dLineRegister       :: C.Carousel,
    dCursorRegister     :: C.Carousel,
    dVideoBuffer        :: M.Memory,
    dCurrentLine        :: Int,
    dVTracker           :: Int,
    dCounter            :: Int
}

new :: IO Terminal
new = do
    vb <- M.fromList M.ReadAccess (replicate (40 * 24) 0xA0)
    vsr <- C.new 960
    lr <- C.new 40
    cb' <- C.new 960
    cb <- execStateT (C.write (0 : replicate 959 1) >> C.shift) cb'
    return $ Terminal { 
                        dCursorChar = 0x00
                      , dChar = 0x00
                      , dVideoShiftRegister = vsr
                      , dLineRegister = lr
                      , dVideoBuffer = vb
                      , dCursorRegister = cb
                      , dCurrentLine = 23
                      , dVTracker = 0
                      , dCounter = 6000}

reset :: StateT Terminal IO ()
reset = do
    term <- liftIO new
    put term

updateChar :: Word8 -> StateT Terminal IO ()
updateChar char = modify (\term -> term {dChar = char})

offsetLine :: Int -> StateT Terminal IO ()
offsetLine offset = modify (\term -> term {dCurrentLine = mod (dCurrentLine term + offset) 24})

offsetCounter :: Int -> StateT Terminal IO ()
offsetCounter offset = modify (\term -> term {dCounter = dCounter term + offset})


offsetVTracker :: Int -> StateT Terminal IO ()
offsetVTracker offset = modify (\term -> term {dVTracker = mod (dVTracker term + offset) 960})

writeChar :: Word8 -> Terminal -> Terminal
writeChar char term = term' where
    currentChar = dChar term
    term' = if currentChar > 0x7F then term else term {dChar = char}

readChar :: Terminal -> Word8
readChar term = currentChar  where
    currentChar = dChar term


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

shiftCB :: StateT Terminal IO Word8
shiftCB = do
    term <- get
    let cb = dCursorRegister term
    (byte, cb') <- liftIO $ runStateT C.shift cb
    put term {dCursorRegister = cb'}
    return byte

shiftNCB :: Int -> StateT Terminal IO ()
shiftNCB n = do
    term <- get
    let cb = dCursorRegister term
    (_, cb') <- liftIO $ runStateT (C.shiftN n) cb
    put term {dCursorRegister = cb'}

writeVSR :: [Word8] -> StateT Terminal IO ()
writeVSR bytes = do
    term <- get
    let vsr = dVideoShiftRegister term
    (_, vsr') <- liftIO $ runStateT (C.write bytes) vsr
    put term {dVideoShiftRegister = vsr'}

shiftNVSR :: Int -> StateT Terminal IO ()
shiftNVSR n = do
    term <- get
    let vsr = dVideoShiftRegister term
    (_, vsr') <- liftIO $ runStateT (C.shiftN n) vsr
    put term {dVideoShiftRegister = vsr'}

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

cr :: StateT Terminal IO ()
cr = do
    vshift <- gets (C.cPosition . dVideoShiftRegister)
    lshift <- gets (C.cPosition . dLineRegister)
    vt <- gets dVTracker

    writeVSR (replicate (41 + mod (-lshift) 40) 0x20) -- Clear the remainder of this line and the entire next line.

    let offset = 1 + mod (-lshift) 40  -- How many characters are left in the current line.
    _ <- shiftNCB offset               -- Move to the start of the next line

    let disp = mod (40 - lshift) 40    -- TBH, I don't really get it. It's how many characters are left in this line.
    when (mod (vshift + disp) 960 == vt) (do -- We check whether after moving to the next line, we overflow and need to scroll
            shiftNVSR (-40)            -- We scroll the screen
            shiftNCB (-40)             -- We scroll the cursor
            offsetVTracker 40          -- We update the vertical tracker
        )

handleKey :: Word8 -> StateT Terminal IO ()
handleKey 0x8D = do
    cr
    simpleShift
    updateChar 0x00
handleKey 0xDF = do
    shiftNCB (-1)
    simpleShift
    updateChar 0x00

handleKey char = do
    pushChar char
    _ <- shiftCB
    updateChar 0x00

pushChar :: Word8 -> StateT Terminal IO ()
pushChar char = do
    vshift <- gets (C.cPosition . dVideoShiftRegister)
    vt <- gets dVTracker
    byte <- pushVSR char
    _ <- pushLR byte
    when (vshift == vt) (do         -- If we are writing to the last character in the screen (and thus need a scroll)
        shiftNVSR (-2)              -- Scroll into position
        writeVSR (replicate 40 0x20)-- Erase entire next line
        shiftNVSR (-38)             -- Scroll the screen (This should be 40, but we already scrolled 2 before)
        shiftNCB (-40)              -- Scroll the cursor
        offsetVTracker 40           -- Update the vertical tracker
        )

pushLine :: [Word8] -> StateT Terminal IO ()
pushLine linebuff = do
    videobuff <- gets dVideoBuffer
    line <- gets dCurrentLine
    let address = [line * 40 + x | x <- [0..39]]
    let linedata = zip address linebuff
    forM_ linedata (\(addr, char) -> do
            let addr' = fromIntegral addr
            liftIO $ M.writeByte videobuff addr' char
            return ()
        )

simpleShift :: StateT Terminal IO ()
simpleShift = do
     byte <- shiftVSR
     _ <- pushLR byte
     return ()


updateCarousel :: StateT Terminal IO ()
updateCarousel = do
    char <- gets dChar
    cursor <- shiftCB
    cursorChar <- gets dCursorChar
    if cursor == 0
        then do
            if char > 0x7F 
                then handleKey char
                else do
                    _ <- shiftVSR
                    _ <- pushLR cursorChar
                    return ()
        else simpleShift


updateDisplay :: StateT Terminal IO ()
updateDisplay = do
    lr <- gets dLineRegister
    let shiftPos = C.cPosition lr
    when (shiftPos == 0) (do
        linebuff <- liftIO $ C.toList lr
        pushLine linebuff
        offsetLine (-1)
        offsetCounter (-1))

updateCursorChar :: StateT Terminal IO ()
updateCursorChar = do
    counter <- gets dCounter
    when (counter < 0) (do
        cursorChar <- gets dCursorChar
        modify (\term -> term {dCursorChar = xor cursorChar 0x20, dCounter = 6000}) -- Hacky number owo
        )

tick' :: StateT Terminal IO ()
tick' = do
    updateCarousel
    updateDisplay
    updateCursorChar

tick :: Terminal -> IO Terminal
tick = execStateT tick'

tickN :: Int -> Terminal -> IO Terminal
tickN n term = do
    let action = replicateM_ n tick'
    execStateT action term
