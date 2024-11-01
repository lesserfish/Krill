module K6502.Types (
      Interface (..)
    , Registers (..)
    , Context (..)
    , FLAG (..)
    , ADDR_MODE (..)
    , K6502 (..)

) where

import Data.Word

data Interface = Interface
    { iReadByte ::  Word16 -> IO Word8
    , iWriteByte :: Word16 -> Word8 -> IO ()
    , iPeekByte ::  Word16 -> IO Word8
    }

data Registers = Registers
    { rIP :: !Word16 -- Program Counter
    , rSP :: !Word8  -- Stack Pointer
    , rAX :: !Word8  -- Accumulator
    , rIX :: !Word8  -- X Register
    , rIY :: !Word8  -- Y Register
    , rFS :: !Word8  -- Flag Status
    }

data Context = Context
    { ctxComplete :: Bool 
    , ctxDecimalEnabled :: Bool
    , ctxSuperInstruction :: Bool
    }
    deriving (Show)

data FLAG
    = CARRY
    | ZERO
    | INTERRUPT_DISABLE
    | DECIMAL_MODE
    | BREAK_CMD
    | OVERFLOW
    | NEGATIVE
    deriving (Show)

data ADDR_MODE
    = IMPLICIT
    | ACCUMULATOR
    | IMMEDIATE
    | ZEROPAGE
    | ZEROPAGE_X
    | ZEROPAGE_Y
    | RELATIVE
    | ABSOLUTE
    | ABSOLUTE_X
    | ABSOLUTE_Y
    | INDIRECT
    | INDIRECT_X
    | INDIRECT_Y
    deriving (Show)

data K6502 = K6502
    { kRegisters :: !Registers
    , kClock :: !Int
    , kCycles :: !Int
    , kContext :: !Context
    }
