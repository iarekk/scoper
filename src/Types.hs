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

data NTree a = Node a [NTree a]
    deriving (Show, Functor, Foldable, Traversable)

-- t = Node 'a' [Node 'b' [Node 'c' [] ], Node 'd' []]
-- t1 = Node "a" [Node "b" [Node "c" [] ], Node "d" []]
-- t = Node 'a' [Node 'b' [], Node 'c' []]

-- data Inorder a = ILeaf
--                | INode (Inorder a) a (Inorder a)  -- as before
--                deriving (Functor, Foldable, Traversable)  -- also using DeriveFunctor and DeriveFoldable

data Preorder a = PrNode a [Preorder a]
                deriving (Functor, Foldable, Traversable)

data Postorder a = PoNode [Postorder a] a
                 deriving (Functor, Foldable, Traversable)

-- injections from the earlier Tree type
-- inorder :: NTree a -> Inorder a
-- inorder Leaf = ILeaf
-- inorder (Node l x r) = INode (inorder l) x (inorder r)

preorder :: NTree a -> Preorder a
--preorder Leaf = PrLeaf
preorder (Node x cs) = PrNode x (map preorder cs)

postorder :: NTree a -> Postorder a
--postorder Leaf = PoLeaf
postorder (Node x cs) = PoNode (map postorder cs) x