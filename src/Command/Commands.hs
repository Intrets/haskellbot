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
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import MessageQueue
import Text.Read (readMaybe)
import Control.Monad.State.Lazy


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
  pureM $ queueMessage "bUrself"
  burselfParrotCommandM

randomFactCommandM :: ConcM App ()
randomFactCommandM = do
  _ <- awaitM_ [ChatCommand "!f", ChatCommand "!fact"] 0
  pureM $ queueMessage =<< randomFact
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
  pureM $ dicegolf m >>= queueMessage . formatDicegolfResult
  dicegolfCommandM

golf :: StringType
golf = TE.decodeUtf8 "\195\162\194\155\194\179"

formatDicegolfResult :: [Int] -> StringType
formatDicegolfResult throws =
  "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
  where t = T.pack . intercalate ", " . map show $ throws











