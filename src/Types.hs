{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module Types where

import           Data.Aeson
import           GHC.Generics

newtype RunOptions =
    RunOptions InputType
    deriving Show

data InputType
    = FromFile FilePath
    | FromStdIn
    deriving Show

type RenderableScopeChildren  = [RenderableScope]
type ScopeChildren  = Maybe [Scope]
type ScopeEnd       = Int
type ScopeHeight    = Int
type ScopeName      = String
type ScopeStart     = Int
type ScopeTop       = Int
type RenderableScopeColour = String

data Scope = Scope
    { name :: ScopeName
    , s    :: ScopeStart
    , e    :: ScopeEnd
    , c    :: ScopeChildren
    } deriving (Generic, Show)
instance ToJSON Scope
instance FromJSON Scope

data RenderableScope = RenderableScope
    { scopeName :: ScopeName
    , start     :: ScopeStart
    , end       :: ScopeEnd
    , children  :: RenderableScopeChildren
    , height    :: ScopeHeight
    , top       :: ScopeTop
    , colour    :: RenderableScopeColour
    } deriving (Generic, Show)
instance ToJSON RenderableScope

data NT a = N a [NT a]
    deriving (Show, Functor, Foldable, Traversable)

-- t = N 'a' [N 'b' [N 'c' [] ], N 'd' []]
-- t1 = N "a" [N "b" [N "c" [] ], N "d" []]
-- t = N 'a' [N 'b' [], N 'c' []]

data Preorder a = PrNode a [Preorder a]
                deriving (Functor, Foldable, Traversable)

data Postorder a = PoNode [Postorder a] a
                 deriving (Functor, Foldable, Traversable)

preorder :: NT a -> Preorder a
preorder (N x cs) = PrNode x (map preorder cs)

postorder :: NT a -> Postorder a
postorder (N x cs) = PoNode (map postorder cs) x

numberNodes :: NT a -> NT (a, Int)
numberNodes t = ti where
    (ti, _) = numberTree t 0

numberElem :: a -> Int -> ((a, Int), Int)
numberElem x i = ((x, i), i + 1)

numberTree :: (NT a) -> Int -> (NT (a, Int), Int)
numberTree (N x xs) i = (N xi xis, i2) where
        (xi, i1) = numberElem x i
        (xis, i2) = numberTrees xs i1

numberTrees :: [NT a] -> Int -> ([NT (a, Int)], Int)
numberTrees [] i = ([], i)
numberTrees (t:ts) i = (ti:tis, i2) where
    (ti, i1) = numberTree t i
    (tis, i2) = numberTrees ts i1


