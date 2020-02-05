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
import System.IO.Unsafe

clean :: StringType -> StringType
clean = T.strip . T.drop 1 . T.dropWhile (/= ':') . T.drop 1

listenM :: ConcM App ()
listenM = do
  handle <- pureM $ asks (botSocket . bot)
  line   <- taskM $ T.hGetLine handle
  let
    message =
      Message (T.words . T.strip . clean $ line) (User 1 "test_user_name")
  runCommandM message
  -- if "PING :" `T.isPrefixOf` line then runCommandM message else runCommandM message 
  listenM

