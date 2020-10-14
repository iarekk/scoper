{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import GHC.Generics

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

-- s <- Data.ByteString.Lazy.readFile "data/tree-like.json" 
-- let scope = Data.Aeson.decode s :: Maybe Scope
-- see this gist for skipping nulls: https://gist.github.com/alanz/2465584