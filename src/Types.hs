{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
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
instance ToJSON Scope where
    toJSON (Scope n s e c) = object $ stripNulls ["name" .= n, "s" .= s, "e" .= e, "c" .= c]
instance FromJSON Scope

stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Null) xs

-- s <- Data.ByteString.Lazy.readFile "data/tree-like.json" 
-- let scope = Data.Aeson.decode s :: Maybe Scope
-- see this gist for skipping nulls: https://gist.github.com/alanz/2465584