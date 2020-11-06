module Converter (preRender, toTree) where

import Types

preRender :: Scope -> RenderableScope
preRender (Scope n s e c) = assignColours (assignTops withHeight) chartColours
    where
        withHeight = RenderableScope n s e newChildren height top ""
        newChildren = mapList $ fmap preRender <$> c
        top = 0 :: ScopeTop
        height = getChildHeights c
        mapList Nothing = []
        mapList (Just xs) = xs

assignColours :: RenderableScope -> [RenderableScopeColour] -> RenderableScope
assignColours (RenderableScope n s e c h t _) (col:cs) = RenderableScope n s e newChildren h t col
    where
        newChildren = colourTheChildren c cs

colourTheChildren :: RenderableScopeChildren -> [RenderableScopeColour] -> RenderableScopeChildren
colourTheChildren [] _ = []
colourTheChildren (ch:chs) (col:cls) = c':(colourTheChildren chs cls)
    where
        RenderableScope n s e c h t _ = ch
        c' = RenderableScope n s e (colourTheChildren c cls) h t col 

assignTops :: RenderableScope -> RenderableScope
assignTops rScope = newScope
    where
        RenderableScope n s e c h t _ = rScope
        newScope = RenderableScope n s e newChildren h t ""
        newChildren = assignTops' t c

assignTops' :: ScopeTop -> RenderableScopeChildren -> RenderableScopeChildren
assignTops' _ [] = []
assignTops' prevTop (x:xs) = x': assignTops' newTop xs
    where
        RenderableScope n s e c h _ _ = x
        x' = RenderableScope n s e (assignTops' (prevTop+1) c) h (prevTop+1) ""
        newTop = prevTop + h

getChildHeights :: ScopeChildren -> ScopeHeight
getChildHeights Nothing = 1
getChildHeights (Just xs) = 1 + sum (map getHeight xs)
    where
        getHeight (Scope _ _ _ c) = getChildHeights c

     
chartColours :: [String]
chartColours = cycle ["#3366cc","#dc3912","#ff9900","#109618","#990099","#0099c6","#dd4477","#66aa00","#b82e2e","#316395","#3366cc","#994499","#22aa99","#aaaa11","#6633cc","#e67300","#8b0707","#651067","#329262","#5574a6","#3b3eac","#b77322","#16d620","#b91383","#f4359e","#9c5935","#a9c413","#2a778d","#668d1c","#bea413","#0c5922","#743411"]

toTree :: Scope -> ScopeTree
toTree (Scope n s e c) = N (ScopeData n s e) (mp c) where
    mp Nothing = []
    mp (Just xs) = map toTree xs