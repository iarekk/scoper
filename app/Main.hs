{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS

import Parsers(runParser)
import Storage(readScope, writeScope)
import Types

main :: IO ()
main = do
    RunOptions dataPath <- runParser
    Prelude.putStrLn $ "Reading file:" ++ dataPath
    -- t <- BS.readFile dataPath
    -- let scope = Data.Aeson.decode t :: Maybe Scope
    scope <- readScope dataPath
    writeScope scope
    

-- 0. Transform the input file into a parse-able file.
-- 1. Parse the file into a Scope
-- 2. Transform the Scope into a renderable structure
-- 3. Generate the svg

