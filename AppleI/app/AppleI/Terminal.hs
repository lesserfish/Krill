    module AppleI.Terminal where

import System.IO (hFlush, stdout)
import Text.Printf
import Data.Word
import Control.Monad.State
import qualified AppleI.Terminal.Carousel as C
import qualified AppleI.Terminal.ThisSucks as FUCK
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

data TState = NORMAL | CLEAR_REQUEST

data Terminal = Terminal {
    dState              :: TState,
    dCursorChar         :: Word8,
    dChar               :: Word8,
    dVideoShiftRegister :: C.Carousel,
    dLineRegister       :: C.Carousel,
    dCursorRegister     :: C.Carousel,
    dVideoBuffer        :: M.Memory,
    dCurrentLine        :: Int,
    dVTracker           :: Int
}

new :: IO Terminal
new = do
    vb <- M.fromList M.ReadAccess (replicate (5 * 5) 0xA0)
    vsr <- C.new 25
    lr <- C.new 5
    cb' <- C.new 25
    cb <- execStateT (C.write (0 : replicate 24 1) >> C.shift) cb'
    return $ Terminal { dState = NORMAL 
                      , dCursorChar = 0x00
                      , dChar = 0x00
                      , dVideoShiftRegister = vsr
                      , dLineRegister = lr
                      , dVideoBuffer = vb
                      , dCursorRegister = cb
                      , dCurrentLine = 4
                      , dVTracker = 0}

updateState :: TState -> StateT Terminal IO ()
updateState st = modify (\term -> term {dState = st})

updateChar :: Word8 -> StateT Terminal IO ()
updateChar char = modify (\term -> term {dChar = char})

offsetLine :: Int -> StateT Terminal IO ()
offsetLine offset = modify (\term -> term {dCurrentLine = mod (dCurrentLine term + offset) 5})

offsetVTracker :: Int -> StateT Terminal IO ()
offsetVTracker offset = modify (\term -> term {dVTracker = mod (dVTracker term + offset) 25})

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
    vt <- gets dVTracker
    lshift <- gets (C.cPosition . dLineRegister)
    let offset = 1 + mod ( - lshift) 5
    writeVSR (replicate (6 + mod (5 - lshift) 5) 0x20)
    _ <- shiftNCB offset
    let disp = mod (5 - lshift) 5
    when (mod (vshift + disp) 25 == vt) (do
            shiftNVSR (-5)
            shiftNCB (-5)
            offsetVTracker 5
        )

handleKey :: Word8 -> StateT Terminal IO ()
handleKey 0xFF = do
    cr
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
    when (vshift == vt) (do
        shiftNVSR (-5)
        shiftNCB (-5)
        offsetVTracker 5
        updateState CLEAR_REQUEST
        )

pushLine :: [Word8] -> StateT Terminal IO ()
pushLine linebuff = do
    videobuff <- gets dVideoBuffer
    line <- gets dCurrentLine
    let address = [line * 5 + x | x <- [0..5]]
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
    if cursor == 0 && char > 0x7F 
        then handleKey char
        else simpleShift


updateDisplay :: StateT Terminal IO ()
updateDisplay = do
    lr <- gets dLineRegister
    let shiftPos = C.cPosition lr
    when (shiftPos == 0) (do
        linebuff <- liftIO $ C.toList lr
        pushLine linebuff
        offsetLine (-1))


normalTick :: StateT Terminal IO ()
normalTick = do
    updateCarousel
    updateDisplay

clearTick :: StateT Terminal IO ()
clearTick = do
    cursor <- shiftCB
    if cursor == 0 
        then do
            writeVSR (replicate 5 0x20)
            shiftNCB (-1)
            updateState NORMAL
        else simpleShift

tick' :: StateT Terminal IO ()
tick' = do
    st <- gets dState
    case st of 
        NORMAL -> normalTick
        CLEAR_REQUEST -> clearTick

tick :: Terminal -> IO Terminal
tick = execStateT tick'

tickN :: Int -> Terminal -> IO Terminal
tickN n term = do
    let action = replicateM_ n tick'
    execStateT action term
