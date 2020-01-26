{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bot.Irc where

import Bot
import Bot.Irc.Send
import Command.Commands

import Conc
import Control.Monad.Reader

import Data.Function ((&))
import qualified Data.Text as T
       (Text, drop, dropWhile, isPrefixOf, pack, strip, unpack, words)
import qualified Data.Text.IO as T (hGetLine)


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
evalC msg = runCommand message
  where
    message = Message (T.words msg) (User 1 "test_user_name")
    t = 1
  -- | "!id" `T.isPrefixOf` msg = end $ privmsg (T.drop 4 msg)
  -- | "!points" `T.isPrefixOf` msg =
  --   case (T.words msg) !? 1 of
  --     Nothing -> end $ privmsg "no name specified"
  --     Just name ->
  --       () & (const $ asks databaseOptions) >>+ getPointsIO name >>- \case
  --         Nothing -> end $ privmsg "nothing"
  --         Just points ->
  --           end $
  --           privmsgS $ printf "user %s has %d points" (T.unpack name) points
  -- | "!give" `T.isPrefixOf` msg =
  --   case (T.words msg) !? 1 of
  --     Nothing -> end $ privmsg "user not found"
  --     Just name ->
  --       () & const (asks databaseOptions) >>+ givePoints 1 (T.unpack name) 1 >>.
  --       End
  -- | "!dicegolf" `T.isPrefixOf` msg =
  --   () &
  --   (const $
  --    case (T.words msg) !? 1 of
  --      Nothing -> dicegolf 100
  --      Just start -> dicegolf (fromMaybe 100 . readMaybe . T.unpack $ start)) >>+
  --   (privmsg . formatDicegolfResult :: [Int] -> App ()) >>.
  --   End
  -- | "!decode" `T.isPrefixOf` msg =
  --   case (T.words msg) !? 1 of
  --     Nothing -> End
  --     Just c -> end $ privmsgS . show . TE.encodeUtf8 $ c
  -- | otherwise = End

