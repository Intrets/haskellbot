module Main where

import Bot
import Bot.Catfacts
import Bot.Irc
import Bot.Irc.Connection
import Bot.Irc.Send
import Bot.Options.Parse
import Command.CursedCommand
import Command.Commands
import MessageQueue

import Control.Exception -- base
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T (unpack)
import GHC.IO.Encoding
import Options.Applicative
import Queue
import System.IO --
import System.Random
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Conc

messageQueue :: MessageQueue
messageQueue = MessageQueue 0 emptyQueue

main :: IO ()
main = do
  setLocaleEncoding utf8
  --hSetBuffering stdout NoBuffering
  config <- parseConfigFile . cfgFile =<< execParser clOptionsParser
  b      <- connect (T.unpack $ ircServer config) (ircPort config)
  man    <- newManager tlsManagerSettings
  let db = Database (dbFile config)
  s   <- getStdGen
  opt <- Options b config db man <$> loadFacts (factsFile config)
  bracket (pure opt) disconnect (loop s)
 where
  disconnect opts = do
    putStrLn "disconnecting"
    hClose . botSocket . bot $ opts
  loop :: StdGen -> Options -> IO ()
  loop s opts = do
    _ <-
      flip runStateT messageQueue
      . flip runStateT  simpleCommands
      . flip runStateT  M.empty
      . flip runStateT  s
      . flip runStateT  M.empty
      . flip runReaderT opts
      $ runApp run
    return ()

run :: App ()
run = do
  botJoin
  runConcM [messageDispensingLoopM, activateTrivia, listenEvent]
