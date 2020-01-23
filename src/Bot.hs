{-# LANGUAGE ConstraintKinds #-}

module Bot where

import Control.Monad.Reader
import GHC.IO.Handle (Handle)
import qualified Network.Socket as N (PortNumber)

import Control.Monad.State.Strict
import System.Random

import qualified Data.Text as T (Text)

import qualified Data.Array as A

data Bot = Bot
  { botSocket :: Handle
  }

data Options = Options
  { bot :: Bot
  , programOptions :: ProgramOptions
  , databaseOptions :: Database
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
  } deriving (Show)

data Database = Database
  { path :: StringType
  } deriving (Show)

type OptionsConfig = MonadReader Options

type RandomGenerator = MonadState StdGen

type StringType = T.Text

newtype App a = App
  { runApp :: StateT StdGen (ReaderT Options IO) a
  } deriving ( Monad
             , Functor
             , Applicative
             , OptionsConfig
             , MonadIO
             , RandomGenerator
             )
