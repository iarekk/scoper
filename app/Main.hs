{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Parsers(runParser)
import Storage(readScope, writeScope)
import Types

main :: IO ()
main = do
    runOptions <- runParser
    scope <- readScope runOptions
    writeScope scope

