module Main(main) where

import Graphics.Blank 
import Converter(preRender)
import Parsers(runParser)
import Storage(readScope, writeRenderableScope)
import Renderer(renderScope)

main :: IO ()
main = do
    runOptions <- runParser
    scope <- readScope runOptions
    let renScope = preRender scope
    writeRenderableScope $ renScope
    blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
            send context $ do                 -- send commands to this specific context
                    renderScope renScope

