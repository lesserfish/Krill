module Main where

import K6502.Disassembler
import qualified Data.ByteString as B
import Data.Word
import Options.Applicative

loadBinary :: FilePath -> IO [Word8]
loadBinary path = B.unpack <$> B.readFile path

data Args = Args
  { inPath       :: FilePath
  , outPath      :: FilePath
  }

argsParser :: Parser Args
argsParser = Args
  <$> strOption
        ( long "in"
       <> short 'i'
       <> metavar "Binary"
       <> help "Path to the 6502 binary file" )
  <*> strOption
        ( long "out"
       <> short 'o'
       <> help "Path to the output file. (Defaults to STDOUT)"
       <> value "" )

processList :: [(Word16, String)] -> String
processList = concatMap (\(_, s) -> s ++ "\n")

writeOut :: FilePath -> String -> IO ()
writeOut "" s = putStrLn s
writeOut fp s = writeFile fp s

main :: IO ()
main = do
    args <- execParser opts 
    dism <- processList <$> (loadBinary (inPath args)  >>= disassembleL')
    writeOut (outPath args) dism
    return ()
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> header "6502 Disassembler" )

