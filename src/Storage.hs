{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage
    ( readScope
    , writeRenderableScope, showScope, getContent, parseValue, parseValuesToTree, parseValueTree, getSlaFromNode
) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (fromJust, fromMaybe, isJust)
import           Data.Scientific            (toBoundedInteger)
import           Data.Text                  (Text, unpack)
import           Data.Tree
import           GHC.Exts                   (sortWith)
import           Text.Read                  (readMaybe)
import           Types                      (InputType (FromFile, FromStdIn),
                                             RenderableScope, RunOptions (..),
                                             ScopeData (ScopeData),
                                             ScopeMetadata (..), ScopeTree)

readScope :: RunOptions -> IO (ScopeTree, ScopeMetadata)
readScope runOptions = do
    content <- getContent runOptions
    let mbScope = parseScopeTree content -- TODO return the error handling
    let meta = parseMetadata content
    case mbScope of
        Nothing    -> error "Error parsing the file"
        Just scope -> return (scope, meta)

parseMetadata :: BS.ByteString -> ScopeMetadata
parseMetadata jsonString = ScopeMetadata sla where
  tree = decode jsonString
  sla = getSlaFromNode tree

getSlaFromNode (Just (Object o)) = sla where
  trySla = getNumber (HM.lookup "sla" o)
  sla = fromMaybe 2000 trySla
getSlaFromNode _ = 2000

getContent :: RunOptions -> IO BS.ByteString
getContent (RunOptions (FromFile dataPath)) = BS.readFile dataPath
getContent (RunOptions FromStdIn)           = BS.getContents

writeRenderableScope :: RenderableScope -> IO ()
writeRenderableScope = print

parseScopeTree :: BS.ByteString -> Maybe ScopeTree
parseScopeTree s = toScopeTree $ parseValuesToTree $ decode s

parseValuesToTree :: Maybe Value -> Tree (Maybe ScopeData)
parseValuesToTree (Just (Object o)) = parsedTree where
    -- totalNodeName = "total"
    -- totalNode = HM.lookup nodeName o
    -- parsedTree = if isJust totalNode
    --   then parseValueTree (nodeName, fromJust totalNode)
    --   else error "can't find the total node"
    subForest = map parseValueTree (HM.toList o)
    parsedTree = Node (Just $ ScopeData "Query" 0 0) subForest
parseValuesToTree _ = Node Nothing []

parseValueTree :: (Text, Value) -> Tree (Maybe ScopeData)
parseValueTree = unfoldTree f where
  f (s, Object o) = (parseValue s o, HM.toList o)
  f _             = (Nothing, [])

parseValue :: Text -> Object -> Maybe ScopeData
parseValue n o = ScopeData (unpack n) <$> s <*> e where
    e = getNumber (HM.lookup "e" o)
    s = getNumber (HM.lookup "s" o)

toScopeTree :: Tree (Maybe ScopeData) -> Maybe ScopeTree
toScopeTree (Node (Just sd) ts) = Just (Node sd ts') where
  ts' = sortWith (\(Node (ScopeData _ s _) _) -> s) $ map (fromJust . toScopeTree) (filter isSomething ts)
toScopeTree _ = Nothing

isSomething :: Tree (Maybe ScopeData) -> Bool
isSomething (Node v _) = isJust v

getNumber :: Maybe Value -> Maybe Int
getNumber (Just (Number sc))   = toBoundedInteger sc
getNumber (Just (String text)) = readMaybe (unpack text)
getNumber _                    = Nothing


showScope :: ScopeTree -> IO()
showScope sc = putStrLn $ drawTree $ fmap show sc
