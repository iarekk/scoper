{-# LANGUAGE OverloadedStrings #-}

module Renderer(renderScope) where
import Graphics.Blank
import qualified Graphics.Blank.Style as GBS
import Types ( RenderableScope(RenderableScope) )
import qualified Data.Text as T
import qualified Data.Colour.SRGB as DCS

renderScope :: RenderableScope -> Canvas()
renderScope (RenderableScope n s e c h t col) = do
    let x = fromIntegral s * 100 + 100
    let y = fromIntegral t * 50 + 100
    let w = fromIntegral (e-s) * 100
    let he = fromIntegral h * 50

    beginPath()
    rect(x, y, w , he)
    GBS.fillStyle $ ((DCS.sRGB24read $ col) :: DCS.Colour Double)
    fill()
    stroke()
    font "20pt Calibri"
    fillStyle "black"
    fillText(T.pack n, x, y)
    stroke()
    mapM (renderScope) c
    stroke()
