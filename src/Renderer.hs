{-# LANGUAGE OverloadedStrings #-}

module Renderer(renderScope) where
import Graphics.Blank
import qualified Data.Colour.SRGB as DCS
import qualified Data.Text as T
import qualified Graphics.Blank.Style as GBS
import Types

renderScope :: RenderableScope -> Canvas()
renderScope (N (RenderableScopeData (ScopeData n s e) h t col) cs) = do
    let rowHeight = 50
    let millisecondToPxRatio = 100
    let topMargin = 20
    let leftMargin = 20
    let x = fromIntegral s * millisecondToPxRatio + leftMargin
    let y = fromIntegral t * rowHeight + topMargin
    let w = fromIntegral (e-s) * millisecondToPxRatio
    let he = fromIntegral h * rowHeight

    beginPath()
    rect(x, y, w, he)
    GBS.fillStyle (DCS.sRGB24read col :: DCS.Colour Double)
    fill()
    mapM_ renderScope cs
    font "20pt Calibri"
    fillStyle "black"
    textBaseline middle
    fillText(T.pack n, x + 5, y + rowHeight/2)