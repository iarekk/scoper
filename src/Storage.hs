{-# LANGUAGE ScopedTypeVariables #-}

module Storage(readScope, writeScope) where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Types

readScope :: FilePath -> IO Scope
readScope path = do
    mbScope <-
        catches (eitherDecode' <$> BS.readFile path)
        [ 
          Handler(\ (e :: IOException) -> return $ Left "Problem reading file")
        , Handler(\ (e :: SomeException) -> return $ Left ("Another problem:" ++ show e))
        ]
    case mbScope of
        Left e -> error $ "Error: " ++ e
        Right scope -> return scope

writeScope :: Scope -> IO ()
writeScope scope = BS.putStrLn $ Data.Aeson.encode scope

