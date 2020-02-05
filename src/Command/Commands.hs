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

-- runCommand :: Message -> Conc App
-- runCommand message =
--   Pure $ do
--     cmd <- getCommand message
--     case cmd of
--       Nothing -> return End
--       Just command -> do
--         time <- liftIO getPOSIXTime
--         onCooldown <- isOnCooldown command (user message) time
--         if onCooldown
--           then do
--             putOnCooldown command (user message)
--             return (action command message)
--           else return End
runCommandA :: Conc2 App Message (Conc2 App Message ())
--runCommandA = Pure2 (\message -> privmsg "hello test") (Id2 (const ()))
runCommandA =
  proc message ->
  do cmd <- Pure2 getCommand (Id2 id) -< message
     case cmd of
         Nothing -> returnA -< Id2 (const ())
         Just command -> Pure2
                           (\ (command, message) ->
                              do time <- liftIO getPOSIXTime
                                 onCooldown <- isOnCooldown command (user message) time
                                 putOnCooldown command (user message)
                                 let m = action command
                                 if onCooldown then return $ Id2 (const ()) else return $ Id2 (const ()))
                           (Id2 id)
                           -< (command, message)

runCommandM :: Message -> ConcM App ()
runCommandM message@(Message text user) = do
  c <- pureM $ do
    liftIO $ print "running command" 
    liftIO $ print text
    cmd <- getCommand message
    case cmd of
      Nothing -> return Nothing
      Just command -> do
        time <- liftIO getPOSIXTime
        onCooldown <- isOnCooldown command user time
        if onCooldown then do
          putOnCooldown command user
          return $ Just $ action command message
        else return Nothing
  case c of
    Nothing -> return ()
    Just cont -> cont

runCommand :: Conc2 App Message ()
runCommand = proc message -> do
  cont <- runCommandA -< message
  returnA -< ()

simpleCommands :: Commands
simpleCommands =
  M.fromList $
  concatMap (\command -> map (, command) (commands command)) commandList

commandList :: [Command App]
commandList =
  [ Command
      "burself parrot"
      ["bUrself"]
      (CommandOptions 2 2 True True (const True))
      burselfParrotCommand
  ]

--   , Command
--       "dicegolf"
--       ["!dicegolf"]
--       (CommandOptions 2 2 True True (const True))
--       dicegolfCommand
--   , Command
--       "facts"
--       ["!fact", "!f", "forsenScoots", "OMGScoots"]
--       (CommandOptions 2 2 True True (const True))
--       (const randomFactCommand)
--   , Command
--       "real facts"
--       ["!realfact", "!rf"]
--       (CommandOptions 2 2 True True (const True))
--       (const randomFactCommand)
--   , Command
--       "dubious facts"
--       ["!dubiousfact", "!df"]
--       (CommandOptions 2 2 True True (const True))
--       (const dubiousFact)
--   , Command
--       "exit"
--       ["!exit"]
--       (CommandOptions 2 2 True True (const True))
--       (const $ Pure $ quit >> return End)
burselfParrotCommand :: Message -> ConcM App ()
burselfParrotCommand = const $ pureM $ (liftIO $ print "123") >> queueMessage "bUrself"

-- randomFactCommand :: Conc App
-- randomFactCommand =
--   Pure $ do
--     fact <- randomFact
--     queueMessage fact
--     return End
-- 
-- dicegolfCommand :: Message -> Conc App
-- dicegolfCommand (Message msg _) =
--   end $ do
--     result <- dicegolf . fromMaybe 100 $ (readMaybe . T.unpack) =<< (msg !? 1)
--     queueMessage . formatDicegolfResult $ result
-- 
-- rollDie :: (Composable n App, RandomGenerator n) => Message -> Conc n
-- rollDie (Message msg _) =
--   () &
--   const
--     (case msg !? 1 of
--        Nothing -> randRange (1 :: Int) 20
--        Just start ->
--          randRange (1 :: Int) (fromMaybe 20 . readMaybe . T.unpack $ start)) >>+
--   (privmsg . T.pack . show :: Int -> App ()) >>.
--   End
-- 
-- golf :: StringType
-- golf = TE.decodeUtf8 "\195\162\194\155\194\179"
-- 
-- formatDicegolfResult :: [Int] -> StringType
-- formatDicegolfResult throws =
--   "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
--   where
--     t = T.pack . intercalate ", " . map show $ throws
