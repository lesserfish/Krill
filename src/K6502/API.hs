module K6502.API where
import K6502.Types


{--
-- Creation

new :: Interface a -> (MOS6502 a)
new interface = MOS6502 reg 0 0 0 ctx interface where
    reg = Registers 0 0 0 0 0 0
    ctx = Context False False False

-- Setters / Getters


tick :: StateT (MOS6502 a, a) IO ()
tick = do
    incClock
    c <- getCycles
    if c > 0
        then do
            updateCycles (-1)
        else do
            incCounter
            opcode <- fetch
            updateCycles (-1) -- Fetch uses one cycle of the instruction
            execute opcode
            setComplete True

tick' :: StateT (MOS6502 a, a) IO Bool
tick' = do
    incClock
    c <- getCycles
    if c > 0
        then do
            updateCycles (-1)
            return False
        else do
            incCounter
            opcode <- fetch
            updateCycles (-1) -- Fetch uses one cycle of the instruction
            execute opcode
            return True


--}
