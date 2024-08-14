module K6502.API ( 
    new,
    tick,
    reset,
    nmi,
    irq
)where

import K6502.Types
import K6502.Internal
import K6502.Execution
import qualified K6502.Instructions (reset, nmi, irq)

import Control.Monad.State
-- Creation

new :: K6502
new = K6502 reg 0 0 ctx where
    reg = Registers 0 0 0 0 0 0
    ctx = Context False False False

-- Setters / Getters

tick :: Interface -> K6502 -> IO K6502
tick interface k = fst <$> execStateT tick' (k, interface)

tick' :: StateT (K6502, Interface) IO ()
tick' = do
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

reset :: Interface -> K6502 -> IO K6502
reset interface k = fst <$> execStateT K6502.Instructions.reset (k, interface)

nmi :: Interface -> K6502 -> IO K6502
nmi interface k = fst <$> execStateT K6502.Instructions.nmi (k, interface)

irq :: Interface -> K6502 -> IO K6502
irq interface k = fst <$> execStateT K6502.Instructions.irq (k, interface)
