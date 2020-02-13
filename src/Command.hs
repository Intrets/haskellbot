module Command where

import Conc
import Bot
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Data.Time.Clock.POSIX

commandLift = App

userLift = App . lift . lift . lift

data Command a = Command
  { name :: StringType
  , commands :: [StringType]
  , options :: CommandOptions
  , action :: Message -> ConcM a ()
  }


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

instance CommandCooldownHandler App where
  isOnCooldown command usr time = do
    u <- checkUserOnCooldown command usr time
    return $ not u
  putOnCooldown command usr = do
    time <- liftIO getPOSIXTime
    userLift $ modify $ M.insert
      usr
      ((time +) $ userCooldown . options $ command)

class (Monad m) =>
      CommandHandler m where
  getCommand :: Message -> m (Maybe (Command App))

commandMapLift = App . lift . lift . lift

instance CommandHandler App where
  getCommand message = case messageWords message of
    [] -> return Nothing
    -- (start : _) -> M.lookup start <$> commandMapLift get
