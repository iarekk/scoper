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

