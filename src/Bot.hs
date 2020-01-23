{-# LANGUAGE ConstraintKinds #-}

module Bot where

import Bot.Options.Parse
import Control.Monad.Reader
import GHC.IO.Handle (Handle)

import Control.Monad.State.Strict
import System.Random

import qualified Data.Array as A

data Bot = Bot
  { botSocket :: Handle
  }

data Options = Options
  { bot :: Bot
  , programOptions :: ProgramOptions
  , databaseOptions :: Database
  , catFacts :: A.Array Int String
  }

data Database = Database
  { path :: String
  } deriving (Show)

type OptionsConfig = MonadReader Options

type RandomGenerator = MonadState StdGen 

newtype App a = App
  { runApp :: StateT StdGen (ReaderT Options IO) a
  } deriving ( Monad
             , Functor
             , Applicative
             , OptionsConfig
             , MonadIO
             , RandomGenerator
             )
