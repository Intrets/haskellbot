module Command where

import Bot
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Data.Time.Clock.POSIX

commandLift = App . lift

userLift = App . lift . lift . lift

class (Monad m) =>
      CommandCooldownHandler m where
  isOnCooldown :: Command a -> User -> POSIXTime -> m Bool
  putOnCooldown :: Command a -> User -> m ()

checkUserOnCooldown :: Command m -> User -> POSIXTime -> App Bool
checkUserOnCooldown command usr time =
  if requireUserCooldown . options $ command
    then do
      userCooldowns <- userLift get
      case (> time) <$> M.lookup usr userCooldowns of
        Nothing -> return False
        Just r  -> return r
    else return False

checkCommandOnCooldown :: Command m -> User -> POSIXTime -> App Bool
checkCommandOnCooldown command usr time =
  if requireGlobalCooldown . options $ command
    then do
      commandCooldowns <- commandLift get
      case (> time) <$> M.lookup (name command) commandCooldowns of
        Nothing -> return False
        Just r  -> return r
    else return False

instance CommandCooldownHandler App where
  isOnCooldown command usr time = do
    u <- checkUserOnCooldown command usr time
    c <- checkCommandOnCooldown command usr time
    return $ not (u && c)
  putOnCooldown command usr = do
    time <- liftIO getPOSIXTime
    commandLift $ modify $ M.insert
      (name command)
      ((time +) $ globalCooldown . options $ command)
    userLift $ modify $ M.insert
      usr
      ((time +) $ userCooldown . options $ command)

class (Monad m) =>
      CommandHandler m where
  getCommand :: Message -> m (Maybe (Command App))

commandMapLift = App . lift . lift . lift . lift

instance CommandHandler App where
  getCommand message = case messageWords message of
    []          -> return Nothing
    (start : _) -> M.lookup start <$> commandMapLift get
