module AppleI.Carousel where

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
    let position' = mod (position + n) (cLength memory)
    put $ memory {cPosition = position'}
    last

shift :: StateT Carousel IO Word8
shift = shiftN 1

insert :: Word8 -> StateT Carousel IO ()
insert word = do
    let d1 = (word .&. 0x01) .>>. 0
    let d2 = (word .&. 0x02) .>>. 1
    let d3 = (word .&. 0x04) .>>. 2
    let d4 = (word .&. 0x08) .>>. 3
    let d5 = (word .&. 0x10) .>>. 4
    let d6 = (word .&. 0x20) .>>. 5

    memory <- get

    let address = mod (cPosition memory - 1) (cLength memory)

    UMV.write (cS1 memory) address d1
    UMV.write (cS2 memory) address d2
    UMV.write (cS3 memory) address d3
    UMV.write (cS4 memory) address d4
    UMV.write (cS5 memory) address d5
    UMV.write (cS6 memory) address d6

push ::  Word8 -> StateT Carousel IO Word8
push word = do
    output <- last
    _ <- shift
    insert word
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
        ) [0 .. (cLength memory - 1)] :: IO [Word8]

new :: Int -> IO Carousel
new len = do
    s1 <- UMV.replicate len 0
    s2 <- UMV.replicate len 0
    s3 <- UMV.replicate len 0
    s4 <- UMV.replicate len 0
    s5 <- UMV.replicate len 0
    s6 <- UMV.replicate len 1
    return $ Carousel {cS1 = s1, cS2 = s2, cS3 = s3, cS4 = s4, cS5 = s5, cS6 = s6, cLength = len, cPosition = 0}
