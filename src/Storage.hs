{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage(readScope, writeScope, writeRenderableScope, parseScopeTree, getValueTree, parseValue, testv) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HM
import           Data.Scientific
import           Types                      (InputType (FromFile, FromStdIn),
                                             RenderableScope, RunOptions (..),
                                             Scope, ScopeData (ScopeData),
                                             ScopeTree)

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

parseValue :: Value -> (Maybe String) -> Maybe ScopeData
parseValue (Object hm) n = sd where
    e = getNumber (HM.lookup "e" hm)
    s = getNumber (HM.lookup "s" hm)
    sd = ScopeData <$> n <*> s <*> e
parseValue _ _ = Nothing

getNumber :: Maybe Value -> Maybe Int
getNumber (Just (Number sc)) = toBoundedInteger sc
getNumber _                  = Nothing

testv =
    ( Object
        ( HM.fromList
            [
              ( "e"
                , Number 550.0
                )
                ,
                ( "s"
                , Number 500.0
                ),
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
                ( "total"
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
            ]
        )
    )
