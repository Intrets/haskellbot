module Main where

import Bot
import Bot.Database.Helpers
import Bot.Irc
import Bot.Irc.Connection
import Bot.Options.Parse

import Control.Exception -- base
import Control.Monad.Reader
import Control.Monad.State.Strict
import Options.Applicative
import System.IO --
import System.Random

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  options <- execParser clOptionsParser
  config <- parseConfigFile $ cfgFile options
  b <- connect (ircServer config) (ircPort config)
  let db = Database (dbFile config)
  let opt = Options b config db
  s <- getStdGen
  bracket (pure opt) disconnect (loop s)
  where
    disconnect options = do
      putStrLn "disconnecting"
      hClose . botSocket . bot $ options
    loop :: StdGen -> Options -> IO ()
    loop s options = do
      a <- (flip runReaderT) options . (flip runStateT) s $ runApp run
      return ()
    --loop s st = (runReaderT (runApp run) $ st)

run :: App ()
run = do
  botJoin
  listen
