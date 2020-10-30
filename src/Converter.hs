module Converter (preRender) where

import Types

preRender :: Scope -> RenderableScope
preRender (Scope n s e c) = RenderableScope n s e newChildren height top
    where
        newChildren = fmap preRender <$> c
        top = 0
        height = getChildHeights c

assignHeights :: Scope -> (Scope, ScopeHeight)
assignHeights scope = (scope, height)
    where
        (Scope _ _ _ c) = scope
        height = getChildHeights c

getChildHeights :: ScopeChildren -> ScopeHeight
getChildHeights Nothing = 1
getChildHeights (Just xs) = sum $ map getHeight xs
    where
        getHeight (Scope _ _ _ c) = getChildHeights c
