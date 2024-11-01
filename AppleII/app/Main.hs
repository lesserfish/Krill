module Main where
import Frontend

import Control.Monad
import Control.Monad.State
import Options.Applicative

loop :: StateT Context IO ()
loop = do
    tick
    control
    renderVBuffer
    exit <- gets exitRequest
    unless exit loop

data Args = Args
  { biosPath       :: FilePath
  , cassettePath   :: FilePath
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

start :: Args -> IO()
start _ = do
    ctx <- initialize
    _ <- execStateT loop ctx
    return ()

main :: IO ()
main = do
    execParser opts >>= start
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "Run the Apple II emulator"
     <> header "Apple II Emulator" )

