{-# LANGUAGE OverloadedStrings #-}

module Renderer(drawDiagram) where
import qualified Data.Colour.SRGB     as DCS
import qualified Data.Text            as T
import           Graphics.Blank
import qualified Graphics.Blank.Style as GBS
import           Types

rowHeight = 50
topMargin = 20
leftMargin = 20
maxHeight = 1000
maxWidth = 1000

drawDiagram :: RenderableScope -> Canvas()
drawDiagram rScope = do
    lineWidth 1
    strokeStyle "black"
    moveTo(leftMargin, topMargin)
    lineTo(leftMargin + maxWidth, topMargin)
    stroke()
    moveTo(leftMargin, topMargin)
    lineTo(leftMargin, topMargin + maxHeight)
    stroke()
    renderScope (getDrawingMetadata rScope) rScope

getDrawingMetadata :: RenderableScope -> DrawingMetadata
getDrawingMetadata t = DrawingMetadata ratio where
    ratio = maxWidth / (fromIntegral maxEnd)
    getEnds = fmap (\ ((RenderableScopeData (ScopeData _ _ e) _ _ _)) -> e) t
    maxEnd = maximum getEnds

renderScope :: DrawingMetadata -> RenderableScope -> Canvas()
renderScope metaData (N (RenderableScopeData (ScopeData n s e) h t col) cs) = do
    let (DrawingMetadata millisecondToPxRatio) = metaData
    let x = fromIntegral s * millisecondToPxRatio + leftMargin
    let y = fromIntegral t * rowHeight + topMargin
    let w = fromIntegral (e-s) * millisecondToPxRatio
    let he = fromIntegral h * rowHeight

    beginPath()
    GBS.fillStyle (DCS.sRGB24read col :: DCS.Colour Double)
    fillRect(x, y, w, he)
    mapM_ (renderScope metaData) cs
    font "20pt Calibri"
    fillStyle "black"
    textBaseline middle
    fillText(T.pack n, x + 5, y + rowHeight/2)
