{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Parsers(runParser)
import Storage(readScope, writeScope)
import Types

main :: IO ()
main = do
    RunOptions dataPath <- runParser
    putStrLn $ "Reading file:" ++ dataPath
    scope <- readScope dataPath
    writeScope scope

-- 0. Transform the input file into a parse-able file.
-- 1. Parse the file into a Scope
-- 2. Transform the Scope into a render-able structure
-- 3. Generate the svg

