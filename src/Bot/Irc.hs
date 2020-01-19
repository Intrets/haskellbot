module Bot.Irc where

import Bot
import Bot.Irc.Send
import Bot.Options.Parse
import Bot.Database.Helpers

import Control.Monad.Reader
import Data.List
import System.IO
import Text.Printf

botJoin :: App ()
botJoin = do
  oauth <- asks (ircOauth . programOptions)
  write "PASS" ("oauth:" ++ oauth)
  nick <- asks (ircNick . programOptions)
  write "NICK" nick
  write "USER" (nick ++ " 0 * :tutorial bot")
  chan <- asks (ircChannel . programOptions)
  write "JOIN" chan


listen :: App ()
listen =
  forever $ do
    h <- asks (botSocket . bot)
    line <- liftIO $ hGetLine h
    liftIO (putStrLn line)
    let s = init line
    if isPing s
      then pong s
      else eval (clean s)
  where
    forever :: App () -> App ()
    forever a = do
      a
      forever a
    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1
    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x
    pong :: String -> App ()
    pong x = write "PONG" (':' : drop 6 x)


(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(a:rest) !? 0 = Just a
(a:rest) !? n = rest !? (n - 1)



-- Dispatch a command
eval :: String -> App ()
eval "!test" = privmsg "hello"
eval "!quit" = quit
eval x
  | "!id " `isPrefixOf` x = privmsg (drop 4 x)
  | "!points" `isPrefixOf` x = do
    case (words x) !? 1 of
      Nothing -> privmsg "user not found"
      Just name -> do
        db <- asks databaseOptions
        points_ <- liftIO $ getPoints db (Right name)
        case points_ of
          Just points -> privmsg $ printf "user %s has %d points" name points
          Nothing -> privmsg $ printf "user %s not found" name
  | "!give" `isPrefixOf` x = do
    case (words x) !? 1 of
      Nothing -> privmsg "user not found"
      Just name -> do
        db <- asks databaseOptions
        liftIO $ givePoints db 1 name 1
eval _ = return () -- ignore everything else

