module Command where

import Bot
import Conc
import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

commandLift = App . lift

userLift = App . lift . lift . lift

class (Monad m) =>
      CommandCooldownHandler m where
  isOnCooldown :: Command a -> User -> Int -> m Bool
  putOnCooldown :: Command a -> User -> Int -> m ()

checkUserOnCooldown :: Command m -> User -> Int -> App Bool
checkUserOnCooldown command user time =
  case (requireUserCooldown . options $ command) of
    True -> do
      userCooldowns <- userLift $ get
      case (< time) <$> M.lookup user userCooldowns of
        Nothing -> return False
        Just r -> return r
    False -> return False

checkCommandOnCooldown :: Command m -> User -> Int -> App Bool
checkCommandOnCooldown command user time =
  case (requireGlobalCooldown . options $ command) of
    True -> do
      commandCooldowns <- commandLift $ get
      case (< time) <$> M.lookup (name command) commandCooldowns of
        Nothing -> return False
        Just r -> return r
    False -> return False

instance CommandCooldownHandler App where
  isOnCooldown command user time = do
    u <- checkUserOnCooldown command user time
    c <- checkCommandOnCooldown command user time
    return $ not (u && c)
  putOnCooldown command user time = do
    return ()
    commandLift $
      modify $ M.insert (name command) (globalCooldown . options $ command)
    userLift $ modify $ M.insert user (userCooldown . options $ command)

class (Monad m) =>
      CommandHandler m where
  getCommand :: Message -> m (Maybe (Command App))

commandMapLift = App . lift . lift . lift . lift

instance CommandHandler App where
  getCommand message =
    case (messageWords message) of
      [] -> return Nothing
      (start:_) -> M.lookup start <$> commandMapLift get

