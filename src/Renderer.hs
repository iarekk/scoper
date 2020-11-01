{-# LANGUAGE OverloadedStrings #-}

module Renderer(renderScope) where
import Graphics.Blank
import Types ( RenderableScope(RenderableScope) )
import qualified Data.Text as T

renderScope :: RenderableScope -> Canvas()
renderScope (RenderableScope n s e c h t) = do
    rect(fromIntegral s*100, fromIntegral t*100, fromIntegral (e-s)*100, fromIntegral h*100)
    font "20pt Calibri"
    fillText(T.pack n, fromIntegral s*100, fromIntegral t*100)
    stroke()
    mapM renderScope c
    stroke()    
