module Main(main) where

import           Converter      (preRender)
import           Graphics.Blank
import           Parsers        (runParser)
import           Renderer       (drawDiagram)
import           Storage        (readScope)

main :: IO ()
main = do
    runOptions <- runParser
    scope <- readScope runOptions
    let renScope = preRender scope
    putStrLn "open http://localhost:3000 to see the diagram"
    blankCanvas 3000 $ \ context -> -- start blank canvas on port 3000
            send context $                  -- send commands to this specific context
                    drawDiagram renScope

