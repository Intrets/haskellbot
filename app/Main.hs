{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bot
import Bot.Catfacts
import Bot.Irc
import Bot.Irc.Connection
import Bot.Options.Parse

import Control.Exception -- base
import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.IO.Encoding
import Options.Applicative
import System.IO --
import System.Random
import qualified Data.Text as T (unpack)

import Conc

main :: IO ()
main = do
  setLocaleEncoding utf8
  --hSetBuffering stdout NoBuffering
  options <- execParser clOptionsParser
  config <- parseConfigFile $ cfgFile options
  b <- connect (T.unpack $ ircServer config) (ircPort config)
  let db = Database (dbFile config)
  s <- getStdGen
  catFacts <- loadFacts $ factsFile config
  let opt = Options b config db catFacts
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
  runConc [listen2]
