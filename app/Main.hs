module Main(main) where

import           Converter      (preRender)
import           Graphics.Blank
import           Parsers        (runParser)
import           Renderer       (drawDiagram)
import           Storage        (readScope, showScope)

main :: IO ()
main = do
    runOptions <- runParser
    (scope,meta) <- readScope runOptions
    showScope scope
    putStrLn ("Meta" ++ show meta)
    let renScope = preRender scope
    putStrLn "open http://localhost:3000?width=2000&height=4000 to see the diagram" -- TODO: put the max height here, should be easy
    blankCanvas 3000 $ \ context -> -- start blank canvas on port 3000
            send context $                  -- send commands to this specific context
                    drawDiagram renScope meta

