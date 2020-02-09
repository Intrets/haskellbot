{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Bot where

-- import Conc
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.IO.Handle (Handle)
import qualified Network.Socket as N (PortNumber)
import Queue

import Control.Monad.State.Strict
import System.Random

import qualified Data.Text as T (Text)

import qualified Data.Array as A
import Data.Time.Clock.POSIX

import Network.HTTP.Client

newtype Bot = Bot
  { botSocket :: Handle
  }

data Options = Options
  { bot :: Bot
  , programOptions :: ProgramOptions
  , databaseOptions :: Database
  , httpsManager :: Manager
  , catFacts :: A.Array Int StringType
  }

data ProgramOptions = ProgramOptions
  { ircServer :: StringType
  , ircPort :: N.PortNumber
  , ircChannel :: StringType
  , ircNick :: StringType
  , ircOauth :: StringType
  , dbFile :: StringType
  , factsFile :: StringType
  }

newtype Database = Database
  { path :: StringType
  }

type OptionsConfig = MonadReader Options

type StringType = T.Text

data User = User
  { userID :: Int
  , displayName :: StringType
  } deriving (Show, Eq)

instance Hashable User where
  hashWithSalt salt = hashWithSalt salt . userID

data Message = Message
  { messageWords :: ![StringType]
  , user :: !User
  } deriving (Show)

data CommandOptions = CommandOptions
  { userCooldown :: !POSIXTime
  , globalCooldown :: !POSIXTime
  , requireUserCooldown :: !Bool
  , requireGlobalCooldown :: !Bool
  , idVerification :: !(Int -> Bool)
  }

type CommandCooldowns = M.HashMap StringType POSIXTime

type UserCooldowns = M.HashMap User POSIXTime

-- type Commands = M.HashMap StringType (Command App)

data MessageQueue = MessageQueue
  { lastMessage :: !POSIXTime
  , queue :: !(Queue StringType)
  }

newtype App a = App
  { runApp :: ReaderT Options (StateT CommandCooldowns (StateT StdGen (StateT UserCooldowns (StateT MessageQueue IO)))) a
  } deriving (Monad, Functor, Applicative, OptionsConfig, MonadIO)

(!?) :: [a] -> Int -> Maybe a
[]         !? _ = Nothing
(a : _   ) !? 0 = Just a
(_ : rest) !? n = rest !? (n - 1)
