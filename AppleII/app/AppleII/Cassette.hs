module AppleII.Cassette (
    Cassette(..)
,   Mode(..)
,   stop
,   pause
,   play
,   setMode
,   setInterface
,   tick
) where

import Data.Word

data Mode = MODE_RECORDING
          | MODE_LISTENING

data Status = STATUS_PLAYING
            | STATUS_STOPPED

data Cassette = Cassette 
    {
        cStatus :: Status
    ,   cMode :: Mode
    ,   cTime :: Int
    ,   cIn :: Int -> IO Word8
    ,   cOut :: Int -> Word8 -> IO ()
    }


stop :: Cassette -> Cassette
stop c = c { cStatus = STATUS_STOPPED, cTime = 0 }

play :: Cassette -> Cassette
play c = c { cStatus = STATUS_PLAYING }

pause :: Cassette -> Cassette
pause c = c { cStatus = STATUS_STOPPED }

setMode :: Mode -> Cassette -> Cassette
setMode m c = c { cMode = m }

setInterface :: (Int -> IO Word8) -> (Int -> Word8 -> IO ()) -> Cassette -> Cassette
setInterface inFunc outFunc c = c { cIn = inFunc, cOut = outFunc }

tick :: Cassette -> IO Cassette
tick c = undefined
