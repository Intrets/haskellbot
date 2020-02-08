{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bot.Irc where

import Conc
import Bot
import Bot.Irc.Send
import Command.Commands
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine)
import Control.Concurrent (threadDelay)
import Control.Monad.State

clean :: StringType -> StringType
clean = T.strip . T.drop 1 . T.dropWhile (/= ':') . T.drop 1

listenM :: ConcM App ()
listenM = do
  handle <- pureM $ asks (botSocket . bot)
  line   <- taskM $ T.hGetLine handle
  forkM [listenM]
  let
    message =
      Message (T.words . T.strip . clean $ line) (User 1 "test_user_name")
  if "PING :" `T.isPrefixOf` line
    then pureM $ pong line
    else runCommandM message

listenEvent :: ConcM App ()
listenEvent = do
  handle <- pureM $ asks (botSocket . bot)
  line   <- taskM $ T.hGetLine handle
  forkM [listenEvent]
  let
    message =
      Message (T.words . T.strip . clean $ line) (User 1 "test_user_name")
  if "PING :" `T.isPrefixOf` line
    then pureM $ pong line
    else case messageWords message of
      []      -> return ()
      (h : _) -> Task (ActionSend (ChatCommand h) ()) EndM



dispatchEventTest :: Int -> ConcM App ()
dispatchEventTest i = do
  Task (ActionSend (ChatCommand "!test") ()) EndM
  taskM $ threadDelay 10000 >> print i
  dispatchEventTest $ succ i

listenEventTest :: ConcM App ()
listenEventTest = do
  let x = 1
  let
    a =
      (\case
        (ChatCommand "!test") -> do
          modify succ
          c <- get
          if c > 100 then return $ Just c else return Nothing
      ) :: (Event -> State Int (Maybe Int))
  let b = ActionAwaitLoop [ChatCommand "!test"] (1 :: Int) a
  Task b EndM
  taskM $ print "!test received"

