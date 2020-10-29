module Converter where

import Types

preRender :: Scope -> RenderableScope
preRender (Scope n s e c) = RenderableScope n s e (fmap preRender <$> c) top height
    where
        top = 0
        height = 0
