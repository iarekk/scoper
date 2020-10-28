{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

newtype RunOptions = RunOptions FilePath deriving Show

type ScopeName = String
type ScopeStart = Int
type ScopeEnd = Int
type ScopeChildren = Maybe [Scope]

data Scope = Scope
    { name    :: ScopeName
    , s       :: ScopeStart
    , e       :: ScopeEnd
    , c       :: ScopeChildren
    } deriving (Generic, Show)
instance ToJSON Scope
instance FromJSON Scope
