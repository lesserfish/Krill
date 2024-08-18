module AppleI.Terminal where

import Text.Printf
import Data.Word
import Control.Monad.State
import Data.Bits (xor)
import qualified AppleI.Carousel as C
import qualified AppleI.Memory as M
import qualified Data.Vector.Generic.Mutable as M

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
                      , dCurrentLine = 0
                      , dCurrentCursor = 0
                      , dState = NORMAL
                      , dCounter = 0}

updateChar :: Word8 -> StateT Terminal IO ()
updateChar char = modify (\term -> term {dChar = char})

updateCursor :: Int -> StateT Terminal IO ()
updateCursor cur = modify (\term -> term {dCurrentCursor = cur})

updateLine :: Int -> StateT Terminal IO ()
updateLine line = modify (\term -> term {dCurrentLine = line})

increaseCounter :: StateT Terminal IO ()
increaseCounter = modify (\term -> term {dCounter = dCounter term + 1})

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

debug :: StateT Terminal IO ()
debug = do
    vsr <- gets dVideoShiftRegister
    vsrdata <- liftIO . C.toList $ vsr
    liftIO . print $ "VSR: " ++ show vsrdata
    liftIO . print $ "P: " ++ show (C.cPosition vsr)
    lr <- gets dLineRegister
    lrdata <- liftIO . C.toList $ lr
    liftIO . print $ "VSR: " ++ show lrdata
    liftIO . print $ "P: " ++ show (C.cPosition lr)

updateCarousel :: StateT Terminal IO ()
updateCarousel = do
    char <- gets dChar
    cursor <- gets dCurrentCursor
    vsr <- gets dVideoShiftRegister
    let shiftPos = C.cPosition vsr
    -- liftIO . print $ "Shift position: " ++ show shiftPos ++ "  Cursor: " ++ show cursor ++ "  Char:  " ++ show char
    if shiftPos == cursor && char > 0x7F
        then do
            byte <- pushVSR char
            _ <- pushLR byte
            let newcursor = mod (cursor + 1) 960
            updateCursor newcursor
            updateChar 0x00
            when (char == 0xB0) debug
        else do
            byte <- shiftVSR
            _ <- pushLR byte
            return ()

pushLine :: [Word8] -> StateT Terminal IO ()
pushLine linebuff = do
    cursor <- gets dCurrentCursor
    videobuff <- gets dVideoBuffer
    line <- gets dCurrentLine
    let address = [line * 40 + x | x <- [0..40]]
    let linedata = zip address linebuff
    forM_ linedata (\(addr, char) -> do
            let addr' = fromIntegral addr
            liftIO $ M.writeByte videobuff addr' char
            return ()
        )
    when (line * 40 <= cursor && cursor <= (line * 40 + 40)) (do
            cursorchar <- gets dCursorChar
            let addr' = fromIntegral cursor
            liftIO $ M.writeByte videobuff addr' cursorchar
        )
    increaseCounter

updateDisplay :: StateT Terminal IO ()
updateDisplay = do
    lr <- gets dLineRegister
    line <- gets dCurrentLine
    let shiftPos = C.cPosition lr
    when (shiftPos == 0) (do
        linebuff <- liftIO $ C.toList lr
        pushLine linebuff
        let newline = mod (line + 1) 24
        updateLine newline
        return ())

updateCursorChar :: StateT Terminal IO ()
updateCursorChar = do
    counter <- gets dCounter
    -- This is a hard coded stupid number that takes into NO account the speed of the actual clock that blinks the cursor.
    -- I think that it is an independent clock but I am not certain. Anyway, we should probably measure it correctly.
    when (mod counter 7000 == 0) (do
        modify (\term -> term {dCursorChar = xor (dCursorChar term) 0x20})
        modify (\term -> term {dCounter = 1})
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
