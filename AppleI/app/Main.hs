module Main where
import Frontend

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
  , ciPath         :: FilePath
  , fontPath       :: FilePath
  }

argsParser :: Parser Args
argsParser = Args
  <$> strOption
        ( long "bios"
       <> short 'b'
       <> metavar "BIOS"
       <> help "Path to the BIOS data"
       <> value "Assets/wozmon.bin"
       <> showDefault )
  <*> strOption
        ( long "cassette"
       <> short 'c'
       <> metavar "CASSETTE"
       <> help "Path to the cassette data"
       <> value "" -- default to empty string
       <> showDefault )
  <*> strOption
        ( long "cinterface"
       <> short 'i'
       <> metavar "CASSETTE INTERFACE"
       <> help "Path to the cassette interface data"
       <> value "Assets/cassette-interface.bin" -- default to empty string
       <> showDefault )
  <*> strOption
        ( long "font"
       <> short 'f'
       <> metavar "FONT"
       <> help "Path to the font file"
       <> value "Assets/font.bmp" -- default font path
       <> showDefault )

start :: Args -> IO()
start args = do
    let fontFP = fontPath args
    biosData <- loadBios (biosPath args)
    ciData <- loadCI (ciPath args)
    cassetteData <- loadCassette (cassettePath args)
    ctx <- initialize biosData ciData cassetteData fontFP
    _ <- execStateT loop ctx
    return ()

main :: IO ()
main = do
    execParser opts >>= start
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "Run the Apple I emulator"
     <> header "Apple I Emulator - a simple Haskell program" )

