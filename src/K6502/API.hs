module K6502.API where

import K6502.Types
import K6502.Internal
import K6502.Execution

import Control.Monad.State
-- Creation

new :: Interface -> K6502
new = K6502 reg 0 0 ctx where
    reg = Registers 0 0 0 0 0 0
    ctx = Context False False False

-- Setters / Getters


tick :: StateT K6502 IO ()
tick = do
    incClock
    c <- getCycles
    if c > 0
        then do
            offsetCycles (-1)
        else do
            opcode <- fetch
            offsetCycles (-1) -- Fetch uses one cycle of the instruction
            execute opcode
            setComplete True
