module AppleI.Terminal.Carousel where

import Text.Printf
import Prelude hiding (head, last)
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Word
import Data.Bits ((.&.), (.>>.), (.<<.))
import Control.Monad.State

data Carousel = Carousel {
    cS1 :: UMV.IOVector Word8,
    cS2 :: UMV.IOVector Word8,
    cS3 :: UMV.IOVector Word8,
    cS4 :: UMV.IOVector Word8,
    cS5 :: UMV.IOVector Word8,
    cS6 :: UMV.IOVector Word8,
    cPosition :: Int,
    cLength :: Int
}

shiftN :: Int -> StateT Carousel IO Word8
shiftN n = do
    memory <- get
    let position = cPosition memory
    let position' = mod (position - n) (cLength memory)
    put $ memory {cPosition = position'}
    head

shift :: StateT Carousel IO Word8
shift = shiftN 1

insert :: Word8 -> StateT Carousel IO ()
insert byte = do
    let d1 = (byte .&. 0x01) .>>. 0
    let d2 = (byte .&. 0x02) .>>. 1
    let d3 = (byte .&. 0x04) .>>. 2
    let d4 = (byte .&. 0x08) .>>. 3
    let d5 = (byte .&. 0x10) .>>. 4
    let d6 = (byte .&. 0x20) .>>. 5

    memory <- get

    let address = mod (cPosition memory) (cLength memory)

    UMV.write (cS1 memory) address d1
    UMV.write (cS2 memory) address d2
    UMV.write (cS3 memory) address d3
    UMV.write (cS4 memory) address d4
    UMV.write (cS5 memory) address d5
    UMV.write (cS6 memory) address d6

write :: [Word8] -> StateT Carousel IO ()
write bytes = do
    memory <- get
    l <- gets cLength
    let bl = length bytes
    let position = cPosition memory - 1
    let addresses = [a | x <- [position .. position + bl - 1], let a = mod x l]
    let writeData = zip addresses bytes
    forM_ writeData (\(address, byte) -> do

            let d1 = (byte .&. 0x01) .>>. 0
            let d2 = (byte .&. 0x02) .>>. 1
            let d3 = (byte .&. 0x04) .>>. 2
            let d4 = (byte .&. 0x08) .>>. 3
            let d5 = (byte .&. 0x10) .>>. 4
            let d6 = (byte .&. 0x20) .>>. 5
            
            UMV.write (cS1 memory) address d1
            UMV.write (cS2 memory) address d2
            UMV.write (cS3 memory) address d3
            UMV.write (cS4 memory) address d4
            UMV.write (cS5 memory) address d5
            UMV.write (cS6 memory) address d6
        )

push ::  Word8 -> StateT Carousel IO Word8
push byte = do
    output <- last
    _ <- shift
    insert byte
    return output

head :: StateT Carousel IO Word8
head = do
    memory <- get
    let address = cPosition memory
    d1 <- UMV.read (cS1 memory) address
    d2 <- UMV.read (cS2 memory) address
    d3 <- UMV.read (cS3 memory) address
    d4 <- UMV.read (cS4 memory) address
    d5 <- UMV.read (cS5 memory) address
    d6 <- UMV.read (cS6 memory) address

    let value = (d1 .<<. 0)
              + (d2 .<<. 1)
              + (d3 .<<. 2)
              + (d4 .<<. 3)
              + (d5 .<<. 4)
              + (d6 .<<. 5)

    return value

last :: StateT Carousel IO Word8
last = do
    memory <- get
    let address = mod (cPosition memory - 1) (cLength memory)
    d1 <- UMV.read (cS1 memory) address
    d2 <- UMV.read (cS2 memory) address
    d3 <- UMV.read (cS3 memory) address
    d4 <- UMV.read (cS4 memory) address
    d5 <- UMV.read (cS5 memory) address
    d6 <- UMV.read (cS6 memory) address

    let value = (d1 .<<. 0)
              + (d2 .<<. 1)
              + (d3 .<<. 2)
              + (d4 .<<. 3)
              + (d5 .<<. 4)
              + (d6 .<<. 5)

    return value

toList :: Carousel -> IO [Word8]
toList memory = do
    let p = cPosition memory
    let l = cLength memory
    let address = [p .. l - 1] ++ [0 .. p - 1]
    mapM (\i -> do 
        d1 <- UMV.read (cS1 memory) i
        d2 <- UMV.read (cS2 memory) i
        d3 <- UMV.read (cS3 memory) i
        d4 <- UMV.read (cS4 memory) i
        d5 <- UMV.read (cS5 memory) i
        d6 <- UMV.read (cS6 memory) i

        let value = (d1 .<<. 0)
                  + (d2 .<<. 1)
                  + (d3 .<<. 2)
                  + (d4 .<<. 3)
                  + (d5 .<<. 4)
                  + (d6 .<<. 5)
        return value
        ) address

toList' :: Carousel -> IO [Word8]
toList' memory = do
    let l = cLength memory
    let address = [0 .. l -1]
    mapM (\i -> do 
        d1 <- UMV.read (cS1 memory) i
        d2 <- UMV.read (cS2 memory) i
        d3 <- UMV.read (cS3 memory) i
        d4 <- UMV.read (cS4 memory) i
        d5 <- UMV.read (cS5 memory) i
        d6 <- UMV.read (cS6 memory) i

        let value = (d1 .<<. 0)
                  + (d2 .<<. 1)
                  + (d3 .<<. 2)
                  + (d4 .<<. 3)
                  + (d5 .<<. 4)
                  + (d6 .<<. 5)
        return value
        ) address



new :: Int -> IO Carousel
new len = do
    s1 <- UMV.generate len (\i -> fromIntegral $ i .&. 1)
    s2 <- UMV.generate len (\i -> fromIntegral $ (i .>>. 1) .&. 1)
    s3 <- UMV.generate len (\i -> fromIntegral $ (i .>>. 2) .&. 1)
    s4 <- UMV.generate len (\i -> fromIntegral $ (i .>>. 3) .&. 1)
    s5 <- UMV.generate len (\i -> fromIntegral $ (i .>>. 4) .&. 1)
    s6 <- UMV.generate len (\i -> fromIntegral $ (i .>>. 5) .&. 1)
    return $ Carousel {cS1 = s1, cS2 = s2, cS3 = s3, cS4 = s4, cS5 = s5, cS6 = s6, cLength = len, cPosition = 0}
