{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module OpentdbHelpers where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8

import GHC.Generics
import Bot
import Command.RenameUtils
import Data.Aeson
import Conc

import qualified Data.ByteString.Base64 as B64

import Data.Text as T
import Data.Either (fromRight)
import Control.Monad.Reader
import Network.HTTP.Client
import Network.HTTP.Types
import Text.Printf

data OpentdbResponse = OpentdbResponse
  { response_code :: Int
  , results :: [TdbResult]
  } deriving (Show, Eq, Generic)

data TdbResult = TdbResult
  { category :: StringTypeSynonym
  , type_ :: StringTypeSynonym
  , difficulty :: StringTypeSynonym
  , question :: StringTypeSynonym
  , correct_answer :: StringTypeSynonym
  , incorrect_answers :: [StringTypeSynonym]
  } deriving (Show, Eq, Generic)

newtype StringTypeSynonym = StringTypeSynonym {unwrap :: StringType } deriving (Show, Eq)

instance FromJSON StringTypeSynonym where
  parseJSON =
    withText "StringType"
      $ return
      . StringTypeSynonym
      . T.pack
      . B8.unpack
      . fromRight ""
      . B64.decode
      . B8.pack
      . T.unpack

instance FromJSON TdbResult where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = ownerFieldRename }

instance FromJSON OpentdbResponse

opentdbQuery :: StringType -> Int -> ConcM App (Maybe [TdbResult])
opentdbQuery queryType (max 1 . min 50 -> count) = do
  man <- pureM $ asks httpsManager
  taskM $ do
    let
      req =
        parseRequest
          (printf
            "http://opentdb.com/api.php?amount=%d&type=%s&encode=base64"
            count
            queryType
          ) :: Maybe Request
    case req of
      Nothing -> return Nothing
      Just r  -> do
        jsonResult <- httpLbs r man
        case statusCode . responseStatus $ jsonResult of
          200 -> do
            let
              response =
                decode (responseBody jsonResult) :: Maybe OpentdbResponse
            return $ fmap results response
          _ -> return Nothing
