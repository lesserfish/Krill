module Main where
import Frontend
import Paths_AppleI (getDataFileName)

import qualified Data.ByteString as B
import Data.Word
import Control.Monad.State
import Options.Applicative

fill :: Int -> [Word8] -> [Word8]
fill n lst
    | length lst == n = lst
    | length lst < n = lst ++ replicate (n - length lst) 0
    | length lst > n = take n lst
    | otherwise = lst

loadBinary :: FilePath -> IO [Word8]
loadBinary path = B.unpack <$> B.readFile path

loadBios :: FilePath -> IO [Word8]
loadBios fp = fill 0x100 <$> loadBinary fp

loadCI :: FilePath -> IO [Word8]
loadCI "" = return $ fill 0x100 []
loadCI fp = fill 0x100 <$> loadBinary fp

loadCassette :: FilePath -> IO [Word8]
loadCassette "" = return $ fill 0x1000 []
loadCassette fp = fill 0x1000 <$> loadBinary fp

loop :: StateT Context IO ()
loop = do
    tick
    control
    exit <- gets exitRequest
    buffer <- getVBuffer
    renderVBuffer buffer
    unless exit loop

data Args = Args
  { biosPath       :: FilePath
  , cassettePath   :: FilePath
  , fontPath       :: FilePath
  }

argsParser :: (String, String, String) -> Parser Args
argsParser (wozmonFP, basicFP, fontFP) = Args
  <$> strOption
        ( long "bios"
       <> short 'b'
       <> metavar "BIOS"
       <> help "Path to the BIOS rom"
       <> value wozmonFP
       <> showDefault )
  <*> strOption
        ( long "basic"
       <> metavar "BASIC"
       <> help "Path to the Basic rom"
       <> value basicFP
       <> showDefault )
  <*> strOption
        ( long "font"
       <> short 'f'
       <> metavar "FONT"
       <> help "Path to the font file"
       <> value fontFP
       <> showDefault )

start :: Args -> IO()
start args = do
    let fontFP = fontPath args
    biosData <- loadBios (biosPath args)
    cassetteData <- loadCassette (cassettePath args)
    ctx <- initialize biosData cassetteData fontFP
    _ <- execStateT loop ctx
    return ()

main :: IO ()
main = do
    wozmonFP <- getDataFileName "Assets/wozmon.bin"
    basicFP <- getDataFileName "Assets/basic.bin"
    fontFP <- getDataFileName "Assets/font.bmp"
    let paths = (wozmonFP, basicFP, fontFP)
    execParser (opts paths) >>= start
  where
    opts paths = info (argsParser paths <**> helper)
      ( fullDesc
     <> progDesc "Run the Apple I emulator"
     <> header "Apple I Emulator - a simple Haskell program" )

