
module Main where

import Bot
import Bot.Database.Helpers
import Bot.Irc.Connection
import Bot.Options.Parse
import Bot.Irc

import Control.Exception -- base
import Control.Monad.Reader
import Options.Applicative
import System.IO --



-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  options <- execParser clOptionsParser
  config <- parseConfigFile $ cfgFile options
  bot <- connect (ircServer config) (ircPort config)
  let db = Database (dbFile config)
  initializeDB db
  let options = Options bot config db
  bracket (pure options) disconnect loop
  where
    disconnect = hClose . botSocket . bot
    loop st = runReaderT (runApp run) st

run :: App ()
run = do
  botJoin
  listen
