module Bot.Irc.Send
  ( privmsg
  , write
  , quit
  ) where

import Bot
import Bot.Options.Parse

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Exit
import System.IO

-- Send a privmsg to the current chan + server
privmsg :: (OptionsConfig m, MonadIO m) => String -> m ()
privmsg msg = do
  chan <- asks (ircChannel . programOptions)
  write "PRIVMSG" (chan ++ " :" ++ msg)

-- Send a message to the server we're currently connected to
write :: (OptionsConfig m, MonadIO m) => String -> String -> m ()
write cmd args = do
  h <- asks (botSocket . bot)
  let msg = cmd ++ " " ++ args ++ "\r\n"
  liftIO $ hPutStr h msg -- Send message on the wire
  liftIO $ putStr ("> " ++ msg) -- Show sent message on the command line

quit :: App ()
quit = write "QUIT" ":Exiting" >> liftIO exitSuccess
