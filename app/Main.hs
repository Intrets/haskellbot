module Main where

import Bot
import Bot.Catfacts
import Bot.Irc
import Bot.Irc.Connection
import Bot.Irc.Send
import Bot.Database.Helpers
import Bot.Options.Parse
import Command.CursedCommand
import Command.Commands
import MessageQueue
import Command.Nam
import Command.Trivia

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

import Control.Concurrent.STM.TBQueue

import Conc

messageQueue' :: MessageQueue
messageQueue' = MessageQueue 0 emptyQueue

main :: IO ()
main = do
  setLocaleEncoding utf8
  config <- parseConfigFile . cfgFile =<< execParser clOptionsParser
  b      <- connect (T.unpack $ ircServer config) (ircPort config)
  man    <- newManager tlsManagerSettings
  let db = Database (dbFile config)
  s      <- getStdGen
  facts  <- loadFacts (factsFile config)
  nams   <- loadNams (namFile config)
  mQueue <- liftIO $ newTBQueueIO 3
  let opt = Options b config db man facts nams mQueue
  bracket (pure opt) disconnect (loop s)
 where
  disconnect opts = do
    putStrLn "disconnecting"
    hClose . botSocket . bot $ opts
  loop :: StdGen -> Options -> IO ()
  loop s opts = do
    _ <-
      flip runStateT messageQueue'
      . flip runStateT  M.empty
      . flip runStateT  s
      . flip runStateT  M.empty
      . flip runReaderT opts
      $ runApp run
    return ()

run :: App ()
run = do
  initializeDB
  botJoin
  runConcM
    [ activateTrivia
    , listenEvent
    , burselfParrotCommandM
    , dicegolfCommandM
    , randomFactCommandM
    , namCountingM
    , messageDispensingLoopM2
    , getPointsM
    , encodeM
    , triviaCommandM
    ]
