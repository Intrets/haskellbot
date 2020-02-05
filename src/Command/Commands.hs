{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Commands where

import Bot

import Bot.Catfacts
import Bot.Irc.Send
import Bot.Random
import Command
import Command.CursedCommand
import Conc
import Control.Monad
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, unpack, words)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import MessageQueue
import Text.Read (readMaybe)

import Control.Arrow
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as M

runCommandM :: Message -> ConcM App ()
runCommandM message@(Message text user) = do
  c <- pureM $ do
    liftIO $ print "running command"
    liftIO $ print text
    cmd <- getCommand message
    case cmd of
      Nothing      -> return Nothing
      Just command -> do
        time       <- liftIO getPOSIXTime
        onCooldown <- isOnCooldown command user time
        if onCooldown
          then do
            putOnCooldown command user
            return $ Just $ action command message
          else return Nothing
  case c of
    Nothing   -> return ()
    Just cont -> cont

simpleCommands :: Commands
simpleCommands = M.fromList
  $ concatMap (\command -> map (, command) (commands command)) commandList

commandList :: [Command App]
commandList =
  [ Command
    "burself parrot"
    ["bUrself"]
    (CommandOptions 2 2 True True (const True))
    burselfParrotCommand
  , Command
    "dicegolf"
    ["!dicegolf"]
    (CommandOptions 2 2 True True (const True))
    dicegolfCommand
  , Command
    "facts"
    ["!fact", "!f", "forsenScoots", "OMGScoots"]
    (CommandOptions 2 2 True True (const True))
    (randomFactCommand)
  , Command
    "real facts"
    ["!realfact", "!rf"]
    (CommandOptions 2 2 True True (const True))
    (randomFactCommand)
  , Command
    "dubious facts"
    ["!dubiousfact", "!df"]
    (CommandOptions 2 2 True True (const True))
    (const dubiousFact)
  ]

burselfParrotCommand :: Message -> ConcM App ()
burselfParrotCommand = const $ pureM $ queueMessage "bUrself"

randomFactCommand :: Message -> ConcM App ()
randomFactCommand = const $ pureM $ do
  fact <- randomFact
  queueMessage fact

dicegolfCommand :: Message -> ConcM App ()
dicegolfCommand (Message msg _) = pureM $ do
  result <- dicegolf . fromMaybe 100 $ (readMaybe . T.unpack) =<< (msg !? 1)
  queueMessage . formatDicegolfResult $ result

rollDie :: Message -> ConcM App ()
rollDie (Message msg _) = do
  roll <- pureM $ case msg !? 1 of
    Nothing -> randRange (1 :: Int) 20
    Just start ->
      randRange (1 :: Int) (fromMaybe 20 . readMaybe . T.unpack $ start)
  pureM $ privmsg . T.pack . show $ roll

golf :: StringType
golf = TE.decodeUtf8 "\195\162\194\155\194\179"

formatDicegolfResult :: [Int] -> StringType
formatDicegolfResult throws =
  "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
  where t = T.pack . intercalate ", " . map show $ throws
