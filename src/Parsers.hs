{-# LANGUAGE OverloadedStrings #-}

module Parsers (runParser) where

import Options.Applicative
import Types

runParser :: IO RunOptions
runParser = execParser (info optionsParser (progDesc "Run scope visualizer application"))

optionsParser :: Parser RunOptions
optionsParser = RunOptions <$> dataPathParser

dataPathParser :: Parser FilePath
dataPathParser = strArgument $
    metavar "INPUT_PATH" <> help "path to input file"