{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Command.Commands where

import Bot

import Command.CursedCommand
import Bot.Catfacts
import Bot.Irc.Send
import Bot.Random
import Command
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

import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as M

runCommand :: Message -> Conc App
runCommand message = Pure $ do
  cmd <- getCommand message
  case cmd of
    Nothing      -> return End
    Just command -> do
      time       <- liftIO getPOSIXTime
      onCooldown <- isOnCooldown command (user message) time
      if onCooldown
        then do
          putOnCooldown command (user message)
          return (action command message)
        else return End

simpleCommands :: Commands
simpleCommands = M.fromList
  $ concatMap (\command -> map (, command) (commands command)) commandList

commandList :: [Command App]
commandList =
  [ Command
    "burself parrot"
    ["bUrself"]
    (CommandOptions 2 2 True True (const True))
    (const burselfParrotCommand)
  , Command
    "dicegolf"
    ["!dicegolf"]
    (CommandOptions 2 2 True True (const True))
    dicegolfCommand
  , Command
    "facts"
    ["!fact", "!f", "forsenScoots", "OMGScoots"]
    (CommandOptions 2 2 True True (const True))
    (const randomFactCommand)
  , Command
    "real facts"
    ["!realfact", "!rf"]
    (CommandOptions 2 2 True True (const True))
    (const randomFactCommand)
  , Command
    "dubious facts"
    ["!dubiousfact", "!df"]
    (CommandOptions 2 2 True True (const True))
    (const dubiousFact)
  ]

burselfParrotCommand :: Conc App
burselfParrotCommand = Pure $ do
  queueMessage "bUrself"
  return End

randomFactCommand :: Conc App
randomFactCommand = Pure $ do
  fact <- randomFact
  queueMessage fact
  return End

dicegolfCommand :: Message -> Conc App
dicegolfCommand (Message msg _) = end $ do
  result <- dicegolf . fromMaybe 100 $ (readMaybe . T.unpack) =<< (msg !? 1)
  queueMessage . formatDicegolfResult $ result

rollDie :: (Composable n App, RandomGenerator n) => Message -> Conc n
rollDie (Message msg _) =
  ()
    &   const
          (case msg !? 1 of
            Nothing -> randRange (1 :: Int) 20
            Just start ->
              randRange (1 :: Int) (fromMaybe 20 . readMaybe . T.unpack $ start)
          )
    >>+ (privmsg . T.pack . show :: Int -> App ())
    >>. End

golf :: StringType
golf = TE.decodeUtf8 "\195\162\194\155\194\179"

formatDicegolfResult :: [Int] -> StringType
formatDicegolfResult throws =
  "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
  where t = T.pack . intercalate ", " . map show $ throws
