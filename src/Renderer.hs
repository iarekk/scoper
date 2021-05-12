{-# LANGUAGE OverloadedStrings #-}

module Renderer(drawDiagram) where
import qualified Data.Colour.SRGB     as DCS
import qualified Data.Text            as T
import           Data.Tree
import           Graphics.Blank
import qualified Graphics.Blank.Style as GBS
import           Types

rowHeight = 25
topMargin = 20
leftMargin = 20
maxHeight = 1500
maxWidth = 1900

drawDiagram :: RenderableScope -> ScopeMetadata -> Canvas()
drawDiagram rScope (ScopeMetadata sla) = do
    -- let (Node (RenderableScopeData _ totalHeight _ _) _) = rScope
    -- let totalHeightInPx = rowHeight * fromIntegral totalHeight
    --eval "NewCanvas(1000,2000);"
    -- eval "var canvas = document.getElementsByTagName('canvas')[0];canvas.width  = 1200;canvas.height = 2500;"
    --newCanvas (1000, 2000)
    fillStyle "white"
    fillRect(0, 0, 2000, 4000)
    let drawMeta = getDrawingMetadata rScope sla
    let (DrawingMetadata millisecondToPxRatio) = drawMeta
    let xSla = fromIntegral sla * millisecondToPxRatio + leftMargin
    lineWidth 1
    strokeStyle "black"
    moveTo(leftMargin, topMargin)
    lineTo(leftMargin + maxWidth, topMargin)
    stroke()
    moveTo(leftMargin, topMargin)
    lineTo(leftMargin, topMargin + maxHeight)
    stroke()
    renderScope drawMeta rScope
    lineWidth 1
    strokeStyle "red"
    moveTo(xSla, 0)
    lineTo(xSla, 4000)
    stroke()
    fillText(T.pack $ show sla, xSla, 20)

getDrawingMetadata :: RenderableScope -> Sla -> DrawingMetadata
getDrawingMetadata t sla = DrawingMetadata ratio where
    ratio = maxWidth / fromIntegral (max maxEnd sla)
    getEnds = fmap (\ (RenderableScopeData (ScopeData _ _ e) _ _ _) -> e) t
    maxEnd = maximum getEnds

renderScope :: DrawingMetadata -> RenderableScope -> Canvas()
renderScope metaData (Node (RenderableScopeData (ScopeData n s e) h t col) cs) = do
    let (DrawingMetadata millisecondToPxRatio) = metaData
    let x = fromIntegral s * millisecondToPxRatio + leftMargin
    let y = fromIntegral t * rowHeight + topMargin
    let w = max 1.0 (fromIntegral (e-s) * millisecondToPxRatio)
    let he = fromIntegral h * rowHeight

    beginPath()
    GBS.fillStyle (DCS.sRGB24read col :: DCS.Colour Double)
    fillRect(x, y, w, he)
    mapM_ (renderScope metaData) cs
    font "12pt Calibri"
    fillStyle "black"
    textBaseline middle
    fillText(T.pack n, x + 5, y + rowHeight/2)
