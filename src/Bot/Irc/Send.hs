{-# LANGUAGE OverloadedStrings #-}
module Bot.Irc.Send
  ( privmsg
  , privmsgS
  , write
  , quit
  ) where

import Bot
import Bot.Options.Parse
import Conc

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Exit
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Monoid ((<>))

-- Send a privmsg to the current chan + server
privmsg :: (OptionsConfig m, MonadIO m) => StringType -> m ()
privmsg msg = do
  chan <- asks (ircChannel . programOptions)
  write "PRIVMSG" (chan <> " :" <> msg)


privmsgS :: (OptionsConfig m, MonadIO m) => String -> m ()
privmsgS = privmsg . T.pack

-- Send a message to the server we're currently connected to
write :: (OptionsConfig m, MonadIO m) => StringType -> StringType -> m ()
write cmd args = do
  h <- asks (botSocket . bot)
  let msg = cmd <> " " <> args <> "\r\n"
  liftIO $ T.hPutStr h msg -- Send message on the wire
  liftIO $ T.putStr ("> " <> msg) -- Show sent message on the command line

quit :: App ()
quit = write "QUIT" ":Exiting" >> liftIO exitSuccess
