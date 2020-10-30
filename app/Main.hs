module Main(main) where

import Converter(preRender)
import Parsers(runParser)
import Storage(readScope, writeRenderableScope)
import Types

main :: IO ()
main = do
    runOptions <- runParser
    scope <- readScope runOptions
    writeRenderableScope $ preRender scope

