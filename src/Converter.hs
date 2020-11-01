module Converter (preRender) where

import Types

preRender :: Scope -> RenderableScope
preRender (Scope n s e c) = assignTops withHeight
    where
        withHeight = RenderableScope n s e newChildren height top
        newChildren = mapList $ fmap preRender <$> c
        top = 0 :: ScopeTop
        height = getChildHeights c
        mapList Nothing = []
        mapList (Just xs) = xs

assignTops :: RenderableScope -> RenderableScope
assignTops rScope = newScope
    where
        RenderableScope n s e c h t = rScope
        newScope = RenderableScope n s e newChildren h t
        newChildren = assignTops' t c

assignTops' :: ScopeTop -> RenderableScopeChildren -> RenderableScopeChildren
assignTops' _ [] = []
assignTops' prevTop (x:xs) = x':(assignTops' newTop xs)
    where
        RenderableScope n s e c h t = x
        x' = RenderableScope n s e (assignTops' prevTop c) h prevTop
        newTop = prevTop + h

getChildHeights :: ScopeChildren -> ScopeHeight
getChildHeights Nothing = 1
getChildHeights (Just xs) = sum $ map getHeight xs
    where
        getHeight (Scope _ _ _ c) = getChildHeights c
