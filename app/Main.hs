{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS

import Lib
import Types

main :: IO ()
main = do
    s <- getLine
    Prelude.putStrLn s
    t <- BS.readFile s
    -- BS.putStrLn t
    let scope = Data.Aeson.decode t :: Maybe Scope
    -- Prelude.putStrLn (show scope)
    BS.putStrLn (Data.Aeson.encode scope)
