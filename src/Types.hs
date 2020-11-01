{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

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

data Scope = Scope
    { name    :: ScopeName
    , s       :: ScopeStart
    , e       :: ScopeEnd
    , c       :: ScopeChildren
    } deriving (Generic, Show)
instance ToJSON Scope
instance FromJSON Scope

data RenderableScope = RenderableScope
    { scopeName      :: ScopeName
    , start     :: ScopeStart
    , end       :: ScopeEnd
    , children  :: RenderableScopeChildren
    , height    :: ScopeHeight
    , top       :: ScopeTop
    } deriving (Generic, Show)
instance ToJSON RenderableScope

data Scp = Scp
    { nm       :: ScopeName
    , ss       :: ScopeStart
    , se       :: ScopeEnd
    } deriving (Generic, Show)
instance ToJSON Scp
--instance FromJSON Scp

data NTree a = Node
    { value:: a
    , childNodes :: [NTree a]
    }
    deriving (Generic, Show)
--instance ToJSON NTree
instance ToJSON a => ToJSON (NTree a)
--instance Show a => Show (NTree a)

