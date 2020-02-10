{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Irc where

import Conc
import Bot
import Bot.Irc.Send
import Command.Commands
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

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

parseTags :: T.Text -> Message
parseTags t = Message (T.words m) (User id' name)
 where
  w = T.split (== ';') t
  name =
    maybe "#display-name not found" (T.tail . T.dropWhile (/= '='))
      . find ("display-name" `T.isPrefixOf`)
      $ w
  id' =
    fromMaybe (0 :: Int)
      . readMaybe
      . maybe "0" (T.unpack . T.tail . T.dropWhile (/= '='))
      . find ("user-id" `T.isPrefixOf`)
      $ w
  m = (!! 2) . T.split (== ':') $ t

listenEvent :: ConcM App ()
listenEvent = do
  handle <- pureM $ asks (botSocket . bot)
  line   <- taskM $ do
    l <- T.hGetLine handle
    print l
    return l
  forkM [listenEvent]
  let message = parseTags line
  if
    | "PING" `T.isPrefixOf` line -> pureM $ pong line
    | ":" `T.isPrefixOf` line -> return ()
    | otherwise -> case messageWords message of
      []      -> return ()
      (h : _) -> do
        sendM (ChatCommand h) (ChatCommandResult message)
        mapM_ (\h' -> sendM (ChatWord h') (ChatCommandResult message))
          $ messageWords message


