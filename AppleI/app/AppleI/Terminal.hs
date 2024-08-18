module AppleI.Terminal where

import Data.Word
import AppleI.Carousel (Carousel)

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
    dChar               :: Word8,
    dVideoShiftRegister :: Carousel,
    dLineRegister       :: Carousel
}

