{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage(readScope, writeScope, writeRenderableScope, parseScopeTree, getValueTree, parseValue, parseValueTree, testv, getTotal, toScopeTree) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HM
import           Data.Scientific
import           Data.Text(Text, unpack)
import           Types                      (InputType (FromFile, FromStdIn),
                                             NT (..), RenderableScope,
                                             RunOptions (..), Scope,
                                             ScopeData (ScopeData), ScopeTree)

readScope :: RunOptions -> IO Scope
readScope runOptions = do
    mbScope <-
        catches (eitherDecode' <$> getContent runOptions)
        [
          Handler(\ (e :: IOException) -> return $ Left "Problem reading file")
        , Handler(\ (e :: SomeException) -> return $ Left ("Another problem:" ++ show e))
        ]
    case mbScope of
        Left e      -> error $ "Error: " ++ e
        Right scope -> return scope

getContent :: RunOptions -> IO BS.ByteString
getContent (RunOptions (FromFile dataPath)) = BS.readFile dataPath
getContent (RunOptions FromStdIn)           = BS.getContents

writeScope :: Scope -> IO ()
writeScope scope = BS.putStrLn $ Data.Aeson.encode scope

writeRenderableScope :: RenderableScope -> IO ()
writeRenderableScope = print

parseScopeTree :: BS.ByteString -> ScopeTree
parseScopeTree = undefined

getValueTree :: BS.ByteString -> Maybe Value
getValueTree s = decode s

parseValue :: Object -> Text -> Maybe ScopeData
parseValue hm n = sd where
    e = getNumber (HM.lookup "e" hm)
    s = getNumber (HM.lookup "s" hm)
    sd = ScopeData (unpack n) <$> s <*> e

parseValueTree :: (Text, Value) -> NT (Maybe ScopeData)
parseValueTree (s, Object o) = N (parseValue o s) chd where
  chd = map parseValueTree (HM.toList o)
parseValueTree (_,_) = N Nothing []

toScopeTree :: NT (Maybe ScopeData) -> ScopeTree
toScopeTree (N (Just sd) ts) = N sd ts' where
  ts' = map toScopeTree (filter isSomething ts)
toScopeTree _ = error "Don't call me on Nothing"

isSomething :: NT (Maybe ScopeData) -> Bool
isSomething (N Nothing _) = False
isSomething _ = True



getNumber :: Maybe Value -> Maybe Int
getNumber (Just (Number sc)) = toBoundedInteger sc
getNumber _                  = Nothing

testv =
    ( Object
        ( HM.fromList
            [
                ( "analytics"
                , Object
                    ( HM.fromList
                        [
                            ( "e"
                            , Number 550.0
                            )
                        ,
                            ( "s"
                            , Number 500.0
                            )
                        ]
                    )
                )
            ,
            getTotal
            ]
        )
    )

getTotal = ( "total"
                , Object
                    ( HM.fromList
                        [
                            ( "database"
                            , Object
                                ( HM.fromList
                                    [
                                        ( "connect"
                                        , Object
                                            ( HM.fromList
                                                [
                                                    ( "e"
                                                    , Number 320.0
                                                    )
                                                ,
                                                    ( "s"
                                                    , Number 210.0
                                                    )
                                                ]
                                            )
                                        )
                                    ,
                                        ( "query"
                                        , Object
                                            ( HM.fromList
                                                [
                                                    ( "e"
                                                    , Number 450.0
                                                    )
                                                ,
                                                    ( "s"
                                                    , Number 320.0
                                                    )
                                                ]
                                            )
                                        )
                                    ,
                                        ( "e"
                                        , Number 500.0
                                        )
                                    ,
                                        ( "s"
                                        , Number 200.0
                                        )
                                    ]
                                )
                            )
                        ,
                            ( "ui"
                            , Object
                                ( HM.fromList
                                    [
                                        ( "e"
                                        , Number 640.0
                                        )
                                    ,
                                        ( "s"
                                        , Number 500.0
                                        )
                                    ]
                                )
                            )
                        ,
                            ( "e"
                            , Number 650.0
                            )
                        ,
                            ( "s"
                            , Number 10.0
                            )
                        ]
                    )
                )
