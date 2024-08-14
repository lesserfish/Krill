module Tester where

import Text.Printf
import Test.Hspec
import Data.Word
import Data.IORef
import qualified Data.Map as M
import K6502
import Loader
import Control.Monad (unless)

data MiniPC = MiniPC {
    minicpu :: K6502,
    miniram :: IORef (M.Map Word16 Word8)
}

readByte :: MiniPC -> Word16 -> IO Word8
readByte pc address = do
    ram <- readIORef . miniram $ pc
    return $ M.findWithDefault 0 address ram

writeByte :: MiniPC -> Word16 -> Word8 -> IO ()
writeByte pc address byte = do
    let ioref = miniram pc
    ram <- readIORef ioref
    let ram' = M.insert address byte ram
    writeIORef ioref ram'

peekByte :: MiniPC -> Word16 -> IO Word8
peekByte = readByte

createInterface :: MiniPC -> Interface
createInterface pc = Interface {iReadByte = readByte pc, iWriteByte = writeByte pc, iPeekByte = peekByte pc}

fromState :: State -> IO MiniPC
fromState state = do
    let memory = M.fromList . ram $ state
    ram <- newIORef memory 
    let empty_cpu = K6502.new
    let registers = Registers {rIP = ip state,
                               rAX = ax state,
                               rIX = ix state,
                               rIY = iy state,
                               rSP = sp state,
                               rFS = fs state}
    let context = (kContext empty_cpu){ctxDecimalEnabled = True}
    let cpu = empty_cpu {kRegisters = registers, kContext = context}
    let pc = MiniPC {minicpu = cpu, miniram = ram}
    return pc

compareCPU :: MiniPC -> MiniPC -> IO Bool
compareCPU correct prediction = do
    let ipc = (rIP . kRegisters . minicpu $ correct) == (rIP . kRegisters . minicpu $ prediction)
    let axc = (rAX . kRegisters . minicpu $ correct) == (rAX . kRegisters . minicpu $ prediction)
    let ixc = (rIX . kRegisters . minicpu $ correct) == (rIX . kRegisters . minicpu $ prediction)
    let iyc = (rIY . kRegisters . minicpu $ correct) == (rIY . kRegisters . minicpu $ prediction)
    let spc = (rSP . kRegisters . minicpu $ correct) == (rSP . kRegisters . minicpu $ prediction)
    let fsc = (rFS . kRegisters . minicpu $ correct) == (rFS . kRegisters . minicpu $ prediction)

    return $ ipc && axc && ixc && iyc && spc && fsc

compareRAM :: MiniPC -> MiniPC -> IO Bool
compareRAM correct prediction = do
    correctRam <- readIORef . miniram $ correct
    predictedRam <- readIORef . miniram $ prediction
    return $ correctRam == predictedRam

comparePC :: MiniPC -> MiniPC -> IO Bool
comparePC correct prediction = do
    cpuCheck <- compareCPU correct prediction
    ramCheck <- compareRAM correct prediction
    return $ cpuCheck && ramCheck

tickPC :: MiniPC -> IO MiniPC
tickPC pc = do
    let interface = createInterface pc
    let cpu = minicpu pc
    cpu' <- tick interface cpu
    return $ pc {minicpu = cpu'}
   

runTest' :: Test -> Spec
runTest' test = describe "K6502 Test" $ do
    let title = name test
    it (show title) $ do
        putStrLn $ "Running test " ++ title 
        initialPC <- fromState . initial $ test
        finalPC <- fromState . final $ test
        predictedPC <- tickPC initialPC
        check <- comparePC finalPC predictedPC
        unless check (debug test initialPC finalPC predictedPC)
        check `shouldBe` True

debug :: Test -> MiniPC -> MiniPC -> MiniPC -> IO()
debug test before correct prediction = do
    let ipb = rIP . kRegisters . minicpu $ before
    let axb = rAX . kRegisters . minicpu $ before
    let ixb = rIX . kRegisters . minicpu $ before
    let iyb = rIY . kRegisters . minicpu $ before
    let spb = rSP . kRegisters . minicpu $ before
    let fsb = rFS . kRegisters . minicpu $ before

    let ipc = rIP . kRegisters . minicpu $ correct
    let axc = rAX . kRegisters . minicpu $ correct
    let ixc = rIX . kRegisters . minicpu $ correct
    let iyc = rIY . kRegisters . minicpu $ correct
    let spc = rSP . kRegisters . minicpu $ correct
    let fsc = rFS . kRegisters . minicpu $ correct

    let ipp = rIP . kRegisters . minicpu $ prediction
    let axp = rAX . kRegisters . minicpu $ prediction
    let ixp = rIX . kRegisters . minicpu $ prediction
    let iyp = rIY . kRegisters . minicpu $ prediction
    let spp = rSP . kRegisters . minicpu $ prediction
    let fsp = rFS . kRegisters . minicpu $ prediction

    printf "B:  IP %04X  AX %02X  IX %02X  IY %02X  SP %02X  FS  %02X\n" ipb axb ixb iyb spb fsb
    printf "C:  IP %04X  AX %02X  IX %02X  IY %02X  SP %02X  FS  %02X\n" ipc axc ixc iyc spc fsc
    printf "P:  IP %04X  AX %02X  IX %02X  IY %02X  SP %02X  FS  %02X\n" ipp axp ixp iyp spp fsp


runTest :: Test -> IO ()
runTest test = hspec $ runTest' test
