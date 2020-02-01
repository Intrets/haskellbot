{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

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
  1
    &   const (asks (botSocket . bot))
    >>+ T.hGetLine
    >>- [listen2]
    >>< (\s ->
          if "PING :" `T.isPrefixOf` s then end $ pong s else evalC (clean s)
        )

evalC :: StringType -> Conc App
evalC "!quit" = end quit
evalC msg     = runCommand message
  where message = Message (T.words . T.strip $ msg) (User 1 "test_user_name")
