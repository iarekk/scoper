module Main(main) where

import Graphics.Blank 
import Converter(preRender, toTree)
import Parsers(runParser)
import Storage(readScope)
import Renderer(renderScope)

main :: IO ()
main = do
    runOptions <- runParser
    scope <- readScope runOptions
    let renScope = preRender scope
    print renScope
    print $ toTree scope
    blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
            send context $ do                 -- send commands to this specific context
                    renderScope renScope

