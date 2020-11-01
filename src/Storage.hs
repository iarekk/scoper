{-# LANGUAGE ScopedTypeVariables #-}

module Storage(readScope, writeScope, writeRenderableScope) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Types

readScope :: RunOptions -> IO Scope
readScope runOptions = do
    mbScope <-
        catches (eitherDecode' <$> getContent runOptions)
        [ 
          Handler(\ (e :: IOException) -> return $ Left "Problem reading file")
        , Handler(\ (e :: SomeException) -> return $ Left ("Another problem:" ++ show e))
        ]
    case mbScope of
        Left e -> error $ "Error: " ++ e
        Right scope -> return scope

getContent :: RunOptions -> IO BS.ByteString
getContent (RunOptions (FromFile dataPath)) = BS.readFile dataPath
getContent (RunOptions FromStdIn) = BS.getContents

writeScope :: Scope -> IO ()
writeScope scope = BS.putStrLn $ Data.Aeson.encode scope

writeRenderableScope :: RenderableScope -> IO ()
writeRenderableScope rscope = BS.putStrLn $ encodePretty rscope


