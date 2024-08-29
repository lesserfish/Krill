module Main where
import Frontend

import qualified Data.ByteString as B
import Data.Word
import Control.Monad.State
import Options.Applicative

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
       <> value "Assets/basic.bin"
       <> showDefault )
  <*> strOption
        ( long "font"
       <> short 'f'
       <> metavar "FONT"
       <> help "Path to the font file"
       <> value "Assets/font.bmp" 
       <> showDefault )

start :: Args -> IO()
start args = do
    ctx <- initialize
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

