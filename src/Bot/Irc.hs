{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bot.Irc where

import Bot
import Bot.Catfacts
import Bot.Database.Helpers
import Bot.Irc.Send
import Bot.Options.Parse
import Bot.Random

import Conc
import Control.Monad.Reader
import Data.List
import Data.Maybe (fromMaybe)
import System.IO
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Text.Encoding as TE

import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Text as T
       (Text, drop, dropWhile, isPrefixOf, pack, strip, unpack, words)
import qualified Data.Text.IO as T (hGetLine)

botJoin :: App ()
botJoin = do
  oauth <- asks (ircOauth . programOptions)
  write "PASS" ("oauth:" <> oauth)
  nick <- asks (ircNick . programOptions)
  write "NICK" nick
  write "USER" (nick <> " 0 * :tutorial bot")
  chan <- asks (ircChannel . programOptions)
  write "JOIN" chan

pong :: StringType -> App ()
pong x = write "PONG" (":" <> T.drop 6 x)

clean :: StringType -> StringType
clean = T.strip . T.drop 1 . T.dropWhile (/= ':') . T.drop 1

listen2 :: Conc App
listen2 =
  1 & const (asks (botSocket . bot)) >>+ T.hGetLine >>- [listen2] >><
  (\s ->
     if ("PING :" `T.isPrefixOf` s)
       then end $ pong s
       else evalC (clean s))


evalC :: StringType -> Conc App
evalC "!test" = end $ (privmsg "test" :: App ())
evalC "!quit" = end $ quit
evalC msg
  | "!id" `T.isPrefixOf` msg = end $ privmsg (T.drop 4 msg)
  | "!points" `T.isPrefixOf` msg =
    case (T.words msg) !? 1 of
      Nothing -> end $ privmsg "no name specified"
      Just name ->
        () & (const $ asks databaseOptions) >>+ getPointsIO name >>- \case
          Nothing -> end $ privmsg "nothing"
          Just points ->
            end $
            privmsgS $ printf "user %s has %d points" (T.unpack name) points
  | "!give" `T.isPrefixOf` msg =
    case (T.words msg) !? 1 of
      Nothing -> end $ privmsg "user not found"
      Just name ->
        () & const (asks databaseOptions) >>+ givePoints 1 (T.unpack name) 1 >>.
        End
  | "!dicegolf" `T.isPrefixOf` msg =
    () &
    (const $
     case (T.words msg) !? 1 of
       Nothing -> dicegolf 100
       Just start -> dicegolf (fromMaybe 100 . readMaybe . T.unpack $ start)) >>+
    (privmsg . formatDicegolfResult :: [Int] -> App ()) >>.
    End
  | "!decode" `T.isPrefixOf` msg =
    case (T.words msg) !? 1 of
      Nothing -> End
      Just c -> end $ privmsgS . show . TE.encodeUtf8 $ c
  | otherwise = End

golf :: StringType
golf = TE.decodeUtf8 "\195\162\194\155\194\179"

formatDicegolfResult :: [Int] -> StringType
formatDicegolfResult throws =
  "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
  where
    t = T.pack . intercalate ", " . map show $ throws
-- Dispatch a command
-- eval :: StringType -> App ()
-- eval "!test" = privmsg "hello"
-- eval "!quit" = quit
-- eval x
--   | "!id " `T.isPrefixOf` x = privmsg (T.drop 4 x)
--   | "!points" `T.isPrefixOf` x = do
--     case (T.words x) !? 1 of
--       Nothing -> privmsg "user not found"
--       Just name -> do
--         db <- asks databaseOptions
--         points_ <- getPoints (Right . T.unpack $ name)
--         case points_ of
--           Just points ->
--             privmsg $ T.pack $ printf "user %s has %d points" name points
--           Nothing -> privmsg $ T.pack $ printf "user %s not found" name
--   | "!give" `T.isPrefixOf` x = do
--     case (T.words x) !? 1 of
--       Nothing -> privmsg "user not found"
--       Just name -> do
--         givePoints 1 (T.unpack name) 1
--   | "!dicegolf" `T.isPrefixOf` x = do
--     case (T.words x) !? 1 of
--       Nothing -> privmsg "user not found"
--       Just start -> do
--         let d = fromMaybe 100 (readMaybe . T.unpack $ start)
--         result <- dicegolf d
--         privmsg . T.pack $ show $ result
--   | "!fact" `T.isPrefixOf` x = randomFact >>= privmsg
--   -- | "!delay" `isPrefixOf` x = do
--   --   case words x of
--   --     (_:delay:msg) -> privmsgDelay (read delay) (unwords msg)
--   --     _ -> return ()
-- eval _ = return () -- ignore everything else
