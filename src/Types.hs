{-# LANGUAGE DeriveFunctor     #-}
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
    ScopeName
    ScopeStart
    ScopeEnd
    RenderableScopeChildren
    ScopeHeight
    ScopeTop
    RenderableScopeColour
    deriving (Show)

data ScopeData = ScopeData
    ScopeName
    ScopeStart
    ScopeEnd
    deriving (Show)

type ScopeTree = NT ScopeData
--type RenderableScope = NT RenderableScopeData

data NT a = N a [NT a]
    deriving (Show, Functor, Traversable, Foldable)

