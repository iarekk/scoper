module Main(main) where

import Graphics.Blank 
import Converter(preRender)
import Parsers(runParser)
import Storage(readScope)
import Renderer(renderScope)

main :: IO ()
main = do
    runOptions <- runParser
    scope <- readScope runOptions
    let renScope = preRender scope
    putStrLn "open http://localhost:3000 to see the diagram"
    blankCanvas 3000 $ \ context -> -- start blank canvas on port 3000
            send context $                  -- send commands to this specific context
                    renderScope renScope

