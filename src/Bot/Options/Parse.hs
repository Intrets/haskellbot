module Bot.Options.Parse where

import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Network.Socket as N
import Options.Applicative
import System.IO
import Data.List.Split (splitOn)

argparser :: Parser String
argparser =
  strOption
    (long "config" <> short 'c' <> metavar "CONFIGFILE" <> value "default.cfg")

data CLOptions = CLOptions
  { cfgFile :: String
  }

clOptions :: Parser CLOptions
clOptions = CLOptions <$> argparser

clOptionsParser :: ParserInfo CLOptions
clOptionsParser =
  info
    (clOptions <**> helper)
    (fullDesc <> progDesc "chat bot test" <> header "this is a header")

data ProgramOptions = ProgramOptions
  { ircServer :: String
  , ircPort :: N.PortNumber
  , ircChannel :: String
  , ircNick :: String
  , ircOauth :: String
  , dbFile :: String
  , factsFile :: String
  } deriving (Show)

parseConfigFile :: String -> IO ProgramOptions
parseConfigFile path = do
  file <- openFile path ReadMode
  contents <- lines <$> hGetContents file
  let getOption =
        (Map.!) . Map.fromList . map ((\[a, b] -> (a, b)) . splitOn "=") $
        contents
  let res =
        ProgramOptions
          (getOption "ircServer")
          (read $ getOption "ircPort")
          (getOption "ircChannel")
          (getOption "ircNick")
          (getOption "ircOauth")
          (getOption "dbFile")
          (getOption "factsFile")
  return res
