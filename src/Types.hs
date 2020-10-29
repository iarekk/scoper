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

type RenderableScopeChildren  = Maybe [RenderableScope]
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
