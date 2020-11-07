module Converter (preRender) where

import           Data.Traversable (mapAccumL)
import           Types

preRender :: Scope -> RenderableScope
preRender scope = fmap toRender treeWithColours
    where
        scopeTree = toTree scope
        treeWithHeights = getHeight scopeTree
        treeWithTops = getTops treeWithHeights 0
        treeWithColours = getColours treeWithTops chartColours

mapList :: Maybe [a] -> [a]
mapList Nothing   = []
mapList (Just xs) = xs

getHeight :: NT ScopeData -> NT (ScopeData, ScopeHeight)
getHeight (N sd ts) = N (sd, chs) nts where
    nts = map getHeight ts
    chs = 1 + (sum $ map gh nts)
    gh (N (_,h) _) = h

-- addChildHeights :: [NT (ScopeData, ScopeHeight)] -> ScopeHeight
-- addChildHeights [] = 0
-- addChildHeights (t:ts) = h + addChildHeights ts where
--     N (_,h) _ = t

getTops :: NT (ScopeData, ScopeHeight) -> ScopeTop -> NT (ScopeData, ScopeHeight, ScopeTop)
getTops t st = snd $ mapAccumL f st t where
    f top (sd, h) = (top+1, (sd, h, top))

getColours :: NT (ScopeData, ScopeHeight, ScopeTop) -> [ScopeColour] -> NT (ScopeData, ScopeHeight, ScopeTop, ScopeColour)
getColours t cols = snd $ mapAccumL f cols t where
    f (c:cs) (sd, h, top) = (cs, (sd, h, top, c))
    f [] _                = error "colours are meant to be infinite"

chartColours :: [ScopeColour]
chartColours = cycle ["#3366cc","#dc3912","#ff9900","#109618","#990099","#0099c6","#dd4477","#66aa00","#b82e2e","#316395","#994499","#22aa99","#aaaa11","#6633cc","#e67300","#8b0707","#651067","#329262","#5574a6","#3b3eac","#b77322","#16d620","#b91383","#f4359e","#9c5935","#a9c413","#2a778d","#668d1c","#bea413","#0c5922","#743411"]

toTree :: Scope -> NT ScopeData
toTree (Scope n s e c) = N (ScopeData n s e) (map toTree (mapList c))

toRender :: (ScopeData, ScopeHeight, ScopeTop, ScopeColour) -> RenderableScopeData
toRender (sd, h, t, c) = RenderableScopeData sd h t c
