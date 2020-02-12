{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command.Commands where

import Bot

import Bot.Catfacts
import Bot.Random
import Command
import Conc
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, unpack, unwords)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import Text.Read (readMaybe)
import Control.Monad.State.Lazy
import Bot.Database.Helpers
import Text.Printf
import qualified Data.ByteString.Char8 as B8

getPointsM :: ConcM App ()
getPointsM = do
  user <- awaitM
    [ChatCommand "!nampoints"]
    0
    (\(EventResult _ (ChatCommandResult m)) -> user m)
  (namPoints, triviaPoints) <- pureM $ fromMaybe (0, 0) <$> getPoints
    (Left $ userID user)
  messageM $ displayName user <> T.pack
    (printf " has %d NaM points and %d trivia points" namPoints triviaPoints)
  getPointsM

runCommandM :: Message -> ConcM App ()
runCommandM message@(Message _ usr) = do
  c <- pureM $ do
    cmd <- getCommand message
    case cmd of
      Nothing      -> return Nothing
      Just command -> do
        time       <- liftIO getPOSIXTime
        onCooldown <- isOnCooldown command usr time
        if onCooldown
          then do
            putOnCooldown command usr
            return $ Just $ action command message
          else return Nothing
  case c of
    Nothing   -> return ()
    Just cont -> cont

burselfParrotCommandM :: ConcM App ()
burselfParrotCommandM = do
  _ <- awaitM_ [ChatCommand "bUrself"] 0
  messageM "bUrself"
  burselfParrotCommandM

randomFactCommandM :: ConcM App ()
randomFactCommandM = do
  _ <- awaitM_ [ChatCommand "!f", ChatCommand "!fact"] 0
  messageM =<< pureM randomFact
  randomFactCommandM

dicegolfCommandM :: ConcM App ()
dicegolfCommandM = do
  m <- fromMaybe 100 <$> awaitM
    [ChatCommand "!dicegolf", ChatCommand "!dg"]
    0
    (\case
      EventResult (ChatCommand _) (ChatCommandResult (Message w _)) ->
        case w of
          (_ : n : _) -> readMaybe . T.unpack $ n :: Maybe Int
          _           -> Nothing
      _ -> Nothing
    )
  pureM (dicegolf m) >>= (messageM . formatDicegolfResult)
  dicegolfCommandM

encodeM :: ConcM App ()
encodeM = do
  Message words _ <- awaitM
    [ChatCommand "!encode"]
    0
    (\(EventResult _ (ChatCommandResult m)) -> m)
  let s = T.unwords . tail $ words
  messageM $ T.pack . show . TE.encodeUtf8 $ s

golf :: StringType
golf = TE.decodeUtf8 "\195\162\194\155\194\179"

formatDicegolfResult :: [Int] -> StringType
formatDicegolfResult throws =
  "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
  where t = T.pack . intercalate ", " . map show $ throws











