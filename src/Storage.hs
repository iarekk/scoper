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
import           Data.Maybe                 (fromJust)
import           Data.Scientific            (toBoundedInteger)
import           Data.Text                  (Text, unpack)
import           Types                      (InputType (FromFile, FromStdIn),
                                             NT (..), RenderableScope,
                                             RunOptions (..),
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
parseScopeTree s = toScopeTree $ parseValuesToTree $ getValueTree s

getValueTree :: BS.ByteString -> Maybe Value
getValueTree s = decode s

parseValuesToTree :: Maybe Value -> NT (Maybe ScopeData)
parseValuesToTree (Just (Object o)) = parsedTree where
    parsedTree = N (Just $ ScopeData "query" 0 1000) (map parseValueTree (HM.toList o))
parseValuesToTree _ = N Nothing []

parseValueTree :: (Text, Value) -> NT (Maybe ScopeData)
parseValueTree (s, Object o) = N (parseValue o s) chd where
  chd = map parseValueTree (HM.toList o)
parseValueTree (_,_) = N Nothing []

parseValue :: Object -> Text -> Maybe ScopeData
parseValue hm n = sd where
    e = getNumber (HM.lookup "e" hm)
    s = getNumber (HM.lookup "s" hm)
    sd = ScopeData (unpack n) <$> s <*> e

toScopeTree :: NT (Maybe ScopeData) -> Maybe ScopeTree
toScopeTree (N (Just sd) ts) = Just (N sd ts') where
  ts' = map (fromJust . toScopeTree) (filter isSomething ts)
toScopeTree _ = Nothing

isSomething :: NT (Maybe ScopeData) -> Bool
isSomething (N Nothing _) = False
isSomething _             = True

getNumber :: Maybe Value -> Maybe Int
getNumber (Just (Number sc)) = toBoundedInteger sc
getNumber _                  = Nothing

