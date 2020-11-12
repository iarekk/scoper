{-# LANGUAGE OverloadedStrings #-}

module Parsers (runParser) where

import           Options.Applicative
import           Types

runParser :: IO RunOptions
runParser = execParser (info optionsParser (progDesc "Run scope visualizer application"))

optionsParser :: Parser RunOptions
optionsParser = RunOptions <$> (fromFileParser <|> fromStdInParser)

fromFileParser :: Parser InputType
fromFileParser = FromFile <$> strArgument (metavar "INPUT_PATH" <> help "path to input file")

fromStdInParser :: Parser InputType
fromStdInParser = flag' FromStdIn $
    long "stdin"
    <> short 's'
    <> help "Read from stdin"
