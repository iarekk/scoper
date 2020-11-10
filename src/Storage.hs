{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage
    (readScope
    , writeRenderableScope
) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Scientific            (toBoundedInteger)
import           Data.Text                  (Text, unpack)
import           Data.Tree
import           Types                      (InputType (FromFile, FromStdIn),
                                             RenderableScope, RunOptions (..),
                                             ScopeData (ScopeData), ScopeTree)

readScope :: RunOptions -> IO ScopeTree
readScope runOptions = do
    mbScope <-
        catches (parseScopeTree <$> getContent runOptions)
        [
          Handler(\ (e :: IOException) -> return Nothing)
        ]
    case mbScope of
        Nothing    -> error $ "Error parsing the file"
        Just scope -> return scope

getContent :: RunOptions -> IO BS.ByteString
getContent (RunOptions (FromFile dataPath)) = BS.readFile dataPath
getContent (RunOptions FromStdIn)           = BS.getContents

writeRenderableScope :: RenderableScope -> IO ()
writeRenderableScope = print

parseScopeTree :: BS.ByteString -> Maybe ScopeTree
parseScopeTree s = toScopeTree $ parseValuesToTree $ decode s

parseValuesToTree :: Maybe Value -> Tree (Maybe ScopeData)
parseValuesToTree (Just (Object o)) = parsedTree where
    nodeName = "total"
    totalNode = HM.lookup nodeName o
    parsedTree = if(not $ isNothing totalNode)
      then parseValueTree (nodeName, fromJust totalNode)
      else error "can't find the total node"
parseValuesToTree _ = Node Nothing []

parseValueTree :: (Text, Value) -> Tree (Maybe ScopeData)
parseValueTree (s, Object o) = Node (parseValue o s) chd where
  chd = map parseValueTree (HM.toList o)
parseValueTree _ = Node Nothing []

-- unfoldTree :: (b -> (a, [b])) -> b -> Tree a
-- (Text,Value) -> (Maybe ScopeData, [(Text,Value)] -> Tree (Maybe ScopeData)

parseValue :: Object -> Text -> Maybe ScopeData
parseValue hm n = sd where
    e = getNumber (HM.lookup "e" hm)
    s = getNumber (HM.lookup "s" hm)
    sd = ScopeData (unpack n) <$> s <*> e

toScopeTree :: Tree (Maybe ScopeData) -> Maybe ScopeTree
toScopeTree (Node (Just sd) ts) = Just (Node sd ts') where
  ts' = map (fromJust . toScopeTree) (filter isSomething ts)
toScopeTree _ = Nothing

isSomething :: Tree (Maybe ScopeData) -> Bool
isSomething (Node v _) = not $ isNothing v

getNumber :: Maybe Value -> Maybe Int
getNumber (Just (Number sc)) = toBoundedInteger sc
getNumber _                  = Nothing

