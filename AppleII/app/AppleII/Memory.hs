module AppleII.Memory (
    Memory(..),
    MemoryType(..),
    fromList,
    readByte,
    writeByte,
    reset,
    reset',
    toList
) where

import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV
import Data.Word

data MemoryType = ReadAccess | ReadOnly

data Memory = RAM {mData :: UMV.IOVector Word8}
            | ROM {mData :: UMV.IOVector Word8}

fromList :: MemoryType -> [Word8] -> IO Memory
fromList ReadAccess memData = do
    umv <- UMV.generate (length memData) (memData !!)
    return $ RAM {mData = umv}
fromList ReadOnly memData = do
    umv <- UMV.generate (length memData) (memData !!)
    return $ ROM {mData = umv}

readByte :: Memory -> Word16 -> IO Word8
readByte (RAM mem) addr = UMV.unsafeRead mem (fromIntegral addr)
readByte (ROM mem) addr = UMV.unsafeRead mem (fromIntegral addr)

writeByte :: Memory -> Word16 -> Word8 -> IO ()
writeByte (RAM mem) addr byte = UMV.unsafeWrite mem (fromIntegral addr) byte
writeByte (ROM _) _ _ = return ()

reset :: Memory -> IO ()
reset (RAM mem) = do
    mapM_ (UMV.modify mem (const 0)) [0..(UMV.length mem - 1)]
reset (ROM _) = return ()

reset' :: Memory -> Word8 -> IO ()
reset' (RAM mem) v = do mapM_ (UMV.modify mem (const v)) [0..(UMV.length mem - 1)]
reset' (ROM _) _ = return ()

toList :: Memory -> IO [Word8]
toList (RAM mem) = do
   fram <- UV.freeze mem 
   return $ UV.toList fram
toList (ROM mem) = do
   fram <- UV.freeze mem 
   return $ UV.toList fram
