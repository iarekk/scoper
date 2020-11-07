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

data NT a = N a [NT a]
    deriving (Show, Functor, Traversable, Foldable)

type MillisecondsToPxRatio = Double
type RenderableScope       = NT RenderableScopeData
type ScopeChildren         = Maybe [Scope]
type ScopeColour           = String
type ScopeEnd              = Int
type ScopeHeight           = Int
type ScopeName             = String
type ScopeStart            = Int
type ScopeTop              = Int

data DrawingMetadata = DrawingMetadata MillisecondsToPxRatio

data Scope = Scope
    { name :: ScopeName
    , s    :: ScopeStart
    , e    :: ScopeEnd
    , c    :: ScopeChildren
    } deriving (Generic, Show)
instance ToJSON Scope
instance FromJSON Scope

data RenderableScopeData = RenderableScopeData
    ScopeData
    ScopeHeight
    ScopeTop
    ScopeColour
    deriving (Show)

data ScopeData = ScopeData
    ScopeName
    ScopeStart
    ScopeEnd
    deriving (Show)
