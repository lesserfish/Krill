module Main where

import qualified AppleI.Carousel as C
import qualified Frontend.Main as App
import Control.Monad.State

main :: IO ()
main = do
    car <- C.new 3
    C.toList car >>= print . show
    (out, car1) <- runStateT (C.push 1) car
    print . show $ out
    C.toList car1 >>= print . show
    C.toList' car1 >>= print . show
    (out, car2) <- runStateT (C.push 1) car1
    print . show $ out
    C.toList car2 >>= print . show
    C.toList' car2 >>= print . show
    App.main
