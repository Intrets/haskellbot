{-# LANGUAGE ConstraintKinds #-}

module Bot where

import Bot.Database.Helpers
import Bot.Options.Parse
import Control.Monad.Reader
import GHC.IO.Handle (Handle)

data Bot = Bot
  { botSocket :: Handle
  }

data Options = Options
  { bot :: Bot
  , programOptions :: ProgramOptions
  , databaseOptions :: Database
  }

type OptionsConfig = MonadReader Options

newtype App a = App
  { runApp :: ReaderT Options IO a
  } deriving (Monad, Functor, Applicative, OptionsConfig, MonadIO)
