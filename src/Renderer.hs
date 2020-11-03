{-# LANGUAGE OverloadedStrings #-}

module Renderer(renderScope) where
import Graphics.Blank
import qualified Graphics.Blank.Style as GBS
import Types ( RenderableScope(RenderableScope) )
import qualified Data.Text as T
import qualified Data.Colour.SRGB as DCS

renderScope :: RenderableScope -> Canvas()
renderScope (RenderableScope n s e c h t col) = do
    let rowHeight = 50
    let millisecondToPxRatio = 100
    let topMargin = 20
    let leftMargin = 20
    let x = fromIntegral s * millisecondToPxRatio + leftMargin
    let y = fromIntegral t * rowHeight + topMargin
    let w = fromIntegral (e-s) * millisecondToPxRatio
    let he = fromIntegral h * rowHeight

    beginPath()
    rect(x, y, w , he)
    GBS.fillStyle $ ((DCS.sRGB24read $ col) :: DCS.Colour Double)
    fill()
    mapM (renderScope) c
    font "20pt Calibri"
    fillStyle "black"
    textBaseline middle
    fillText(T.pack n, x + 5, y + rowHeight/2)