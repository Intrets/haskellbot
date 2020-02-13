{-# LANGUAGE OverloadedStrings #-}

module Bot.Options.Parse where

import Bot
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Options.Applicative

import qualified Data.Text as T
import qualified Data.Text.IO as T

argparser :: Parser StringType
argparser = T.pack <$> strOption
  (long "config" <> short 'c' <> metavar "CONFIGFILE" <> value "default.cfg")

data CLOptions = CLOptions
  { cfgFile :: StringType
  }

clOptions :: Parser CLOptions
clOptions = CLOptions <$> argparser

clOptionsParser :: ParserInfo CLOptions
clOptionsParser = info
  (clOptions <**> helper)
  (fullDesc <> progDesc "chat bot test" <> header "this is a header")

parseConfigFile :: StringType -> IO ProgramOptions
parseConfigFile pth = do
  contents <- T.lines <$> T.readFile (T.unpack pth)
  let
    getOption =
      (Map.!)
        . Map.fromList
        . map ((\[a, b] -> (a, b)) . T.splitOn "=")
        $ contents
  let
    res = ProgramOptions
      (getOption "ircServer")
      (read . T.unpack $ getOption "ircPort")
      (getOption "ircChannel")
      (getOption "ircNick")
      (getOption "ircOauth")
      (getOption "clientID")
      (getOption "dbFile")
      (getOption "factsFile")
      (getOption "namFile")
  return res
