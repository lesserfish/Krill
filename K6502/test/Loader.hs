{-# LANGUAGE DeriveGeneric #-}

module Loader where

import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Aeson
import GHC.Generics

data State = State {
    ip :: Word16,
    sp :: Word8,
    ax :: Word8,
    ix :: Word8,
    iy :: Word8,
    fs :: Word8,
    ram :: [(Word16, Word8)]
} deriving (Generic, Show)

stateName :: String -> String
stateName "ip" = "pc"
stateName "sp" = "s"
stateName "ax" = "a"
stateName "ix" = "x"
stateName "iy" = "y"
stateName "fs" = "p"
stateName x = x

data Test = Test {
    name :: String,
    initial :: State,
    final :: State
} deriving (Generic, Show)

newtype TestSet = TestSet [Test] deriving (Generic, Show)

instance FromJSON State where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = stateName }
instance FromJSON Test
instance FromJSON TestSet


load :: String -> IO TestSet
load filepath = do
    json <- B.readFile filepath
    let decoded = eitherDecode json
    case decoded of
        Left err_msg -> error ("Could not parse JSON file to TestSet: " ++ err_msg)
        Right output -> return output
