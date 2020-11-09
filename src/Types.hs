{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module Types where

import Data.Tree

newtype RunOptions =
    RunOptions InputType
    deriving Show

data InputType
    = FromFile FilePath
    | FromStdIn
    deriving Show

type MillisecondsToPxRatio = Double
type RenderableScope       = Tree RenderableScopeData
type ScopeColour           = String
type ScopeEnd              = Int
type ScopeHeight           = Int
type ScopeName             = String
type ScopeStart            = Int
type ScopeTop              = Int
type ScopeTree             = Tree ScopeData

data DrawingMetadata = DrawingMetadata MillisecondsToPxRatio
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
