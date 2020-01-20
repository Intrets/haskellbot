{-# LANGUAGE ConstraintKinds #-}

module Bot where

--import Bot.Database.Helpers
import Bot.Options.Parse
import Control.Monad.Reader
import GHC.IO.Handle (Handle)

import Control.Monad.State.Strict
import System.Random

data Bot = Bot
  { botSocket :: Handle
  }

data Options = Options
  { bot :: Bot
  , programOptions :: ProgramOptions
  , databaseOptions :: Database
  }

data Database = Database
  { path :: String
  } deriving (Show)

type OptionsConfig = MonadReader Options

newtype App a = App
  { runApp :: StateT StdGen (ReaderT Options IO) a
  } deriving ( Monad
             , Functor
             , Applicative
             , OptionsConfig
             , MonadIO
             , MonadState StdGen
             )
-- newtype App a = App
--   { runApp :: ReaderT Options IO a
--   } deriving (Monad, Functor, Applicative, OptionsConfig, MonadIO)
