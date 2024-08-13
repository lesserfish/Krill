module Utils where

import Control.Monad.State
import Data.Bits
import Data.Word
import Data.Int

modifyFst :: (a -> a) -> StateT (a, b) IO ()
modifyFst f = do
    (s1, s2) <- get
    let s1' = f s1
    put (s1', s2)

-- This may or may not affect performance
b0 :: Bits a => a -> Bool
b0 x = testBit x 0
b1 :: Bits a => a -> Bool
b1 x = testBit x 1
b2 :: Bits a => a -> Bool
b2 x = testBit x 2
b3 :: Bits a => a -> Bool
b3 x = testBit x 3
b4 :: Bits a => a -> Bool
b4 x = testBit x 4
b5 :: Bits a => a -> Bool
b5 x = testBit x 5
b6 :: Bits a => a -> Bool
b6 x = testBit x 6
b7 :: Bits a => a -> Bool
b7 x = testBit x 7
b8 :: Bits a => a -> Bool
b8 x = testBit x 8
b9 :: Bits a => a -> Bool
b9 x = testBit x 9
b10 :: Bits a => a -> Bool
b10 x = testBit x 10
b11 :: Bits a => a -> Bool
b11 x = testBit x 11
b12 :: Bits a => a -> Bool
b12 x = testBit x 12
b13 :: Bits a => a -> Bool
b13 x = testBit x 13
b14 :: Bits a => a -> Bool
b14 x = testBit x 14
b15 :: Bits a => a -> Bool
b15 x = testBit x 15


joinBytes :: Word8 -> Word8 -> Word16
joinBytes hb lb = (fromIntegral hb .<<. 8) .|. fromIntegral lb

splitBytes :: Word16 -> (Word8, Word8)
splitBytes byte = (hb, lb)
  where
    lb = fromIntegral (0x00FF .&. byte)
    hb = fromIntegral ((byte .>>. 8) .&. 0x00FF)

toU8 x = (fromIntegral x) :: Word8
toU16 x = (fromIntegral x) :: Word16
