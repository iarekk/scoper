module Converter (preRender, toTree, getHeight, getTops, getColours) where

import qualified Data.Traversable as DT
import           Types

preRender :: Scope -> RenderableScope
preRender scope = fmap toRender treeWithColours
    where
        scopeTree = toTree scope
        treeWithHeights = getHeight scopeTree
        treeWithTops = getTops treeWithHeights 0
        treeWithColours = getColours treeWithTops chartColours

        --(Scope n s e c) = scope
        -- h = 0
        -- t = 0
        -- col = "#ff00ff"
        -- rc = []


mapList :: Maybe [a] -> [a]
mapList Nothing   = []
mapList (Just xs) = xs

getHeight :: NT ScopeData -> NT (ScopeData, ScopeHeight)
getHeight (N sd ts) = N (sd, chs) nts where
    nts = map getHeight ts
    chs = addChildHeights nts

addChildHeights :: [NT (ScopeData, ScopeHeight)] -> ScopeHeight
addChildHeights [] = 1
addChildHeights (t:ts) = h + addChildHeights ts where
    N (_,h) _ = t

getTops :: NT (ScopeData, ScopeHeight) -> ScopeTop -> NT (ScopeData, ScopeHeight, ScopeTop)
getTops t st = snd $ DT.mapAccumL f st t where
    f top (sd, h) = (top+1, (sd, h, top))

-- getTops' :: NT (ScopeData, ScopeHeight) -> ScopeTop -> NT (ScopeData, ScopeHeight, ScopeTop)
-- getTops' t curTop = N (sd, h, curTop) nts
--     where 
--         N (sd, h) ts = t
--         newTop = curTop + h


getColours :: NT (ScopeData, ScopeHeight, ScopeTop) -> [ScopeColour] -> NT (ScopeData, ScopeHeight, ScopeTop, ScopeColour)
getColours t cols = snd $ DT.mapAccumL f cols t where
    f (c:cs) (sd, h, top) = (cs, (sd, h, top, c))
    f [] _ = error "colours are meant to be infinite"



--getHeight t ts 
    -- map getHeight where
    -- getHeight (N t c) = N (t, 0)
    -- --getHeight (N _ xs) = undefined

-- assignColours :: RenderableScope -> [RenderableScopeColour] -> RenderableScope
-- assignColours (RenderableScope n s e c h t _) (col:cs) = RenderableScope n s e newChildren h t col
--     where
--         newChildren = colourTheChildren c cs

-- colourTheChildren :: RenderableScopeChildren -> [RenderableScopeColour] -> RenderableScopeChildren
-- colourTheChildren [] _ = []
-- colourTheChildren (ch:chs) (col:cls) = c':(colourTheChildren chs cls)
--     where
--         RenderableScope n s e c h t _ = ch
--         c' = RenderableScope n s e (colourTheChildren c cls) h t col

-- assignTops :: RenderableScope -> RenderableScope
-- assignTops rScope = newScope
--     where
--         RenderableScope n s e c h t _ = rScope
--         newScope = RenderableScope n s e newChildren h t ""
--         newChildren = assignTops' t c

-- assignTops' :: ScopeTop -> RenderableScopeChildren -> RenderableScopeChildren
-- assignTops' _ [] = []
-- assignTops' prevTop (x:xs) = x': assignTops' newTop xs
--     where
--         RenderableScope n s e c h _ _ = x
--         x' = RenderableScope n s e (assignTops' (prevTop+1) c) h (prevTop+1) ""
--         newTop = prevTop + h

-- getChildHeights :: ScopeChildren -> ScopeHeight
-- getChildHeights Nothing = 1
-- getChildHeights (Just xs) = 1 + sum (map getHeight xs)
--     where
--         getHeight (Scope _ _ _ c) = getChildHeights c


chartColours :: [ScopeColour]
chartColours = cycle ["#3366cc","#dc3912","#ff9900","#109618","#990099","#0099c6","#dd4477","#66aa00","#b82e2e","#316395","#994499","#22aa99","#aaaa11","#6633cc","#e67300","#8b0707","#651067","#329262","#5574a6","#3b3eac","#b77322","#16d620","#b91383","#f4359e","#9c5935","#a9c413","#2a778d","#668d1c","#bea413","#0c5922","#743411"]

toTree :: Scope -> NT ScopeData
toTree (Scope n s e c) = N (ScopeData n s e) (map toTree (mapList c))

toRender :: (ScopeData, ScopeHeight, ScopeTop, ScopeColour) -> RenderableScopeData
toRender (sd, h, t, c) = RenderableScopeData sd h t c
