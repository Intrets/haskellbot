{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Irc where

import Bot
import Bot.Irc.Send
import Command.Commands
import Conc
import qualified Conc
import Control.Monad.Reader
import Data.Function ((&))
import qualified Data.Text as T
       (Text, drop, dropWhile, isPrefixOf, pack, strip, unpack, words)
import qualified Data.Text.IO as T (hGetLine)
import GHC.IO.Handle (Handle)

clean :: StringType -> StringType
clean = T.strip . T.drop 1 . T.dropWhile (/= ':') . T.drop 1

listen2 :: Conc App
listen2 =
  1 & const (asks (botSocket . bot)) >>+ T.hGetLine >>-- [listen2] >><
  (\s ->
     if "PING :" `T.isPrefixOf` s
       then end $ pong s
       else evalC (clean s))

listen2Test :: Conc2 App t ()
listen2Test =
  proc x ->
  do handle <- Pure2 (const (asks (botSocket . bot) :: App Handle))
                 (Id2 id)
                 -< x
     line <- IOtask2 T.hGetLine (Id2 id) -< handle
     let message
           = Message (T.words . T.strip $ line) (User 1 "test_user_name")
 --    if "PING :" `T.isPrefixOf` line then
 --      Pure2 pong (Id2 $ const ()) -< line else runCommandA -< message
     listen2Test -< x

evalC :: StringType -> Conc App
evalC "!quit" = end quit
evalC msg = undefined -- runCommand message
  where
    message = Message (T.words . T.strip $ msg) (User 1 "test_user_name")
