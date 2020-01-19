module Bot.Irc.Send
  ( privmsg
  , write
  , quit
  ) where

import Bot
import Bot.Options.Parse

import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Exit
import System.IO

-- Send a privmsg to the current chan + server
privmsg :: String -> App ()
privmsg msg = do
  chan <- asks (ircChannel . programOptions)
  write "PRIVMSG" (chan ++ " :" ++ msg)

-- Send a message to the server we're currently connected to
write :: String -> String -> App ()
write cmd args = do
  h <- asks (botSocket . bot)
  let msg = cmd ++ " " ++ args ++ "\r\n"
  liftIO $ hPutStr h msg -- Send message on the wire
  liftIO $ putStr ("> " ++ msg) -- Show sent message on the command line

quit :: App ()
quit = write "QUIT" ":Exiting" >> liftIO exitSuccess


