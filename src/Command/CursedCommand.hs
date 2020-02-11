{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command.CursedCommand
  ( FactDbResponse()
  , FactResult()
  , activateTrivia
  )
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8

import GHC.Generics
import Bot
import Command.RenameUtils
import Conc
import Data.Aeson

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import qualified Data.ByteString.Base64 as B64

import MessageQueue
import Data.Text as T
import qualified Data.List as L (head)
import Data.Either (fromRight)

import Control.Monad.Reader

activateTrivia :: ConcM App ()
activateTrivia = do
  _ <- awaitM_ [ChatCommand "!df"] 0
  dubiousFactTrivia

dubiousFactTrivia :: ConcM App ()
dubiousFactTrivia = do
  man   <- pureM $ asks httpsManager
  fetch <- taskM $ do
    let req = "http://opentdb.com/api.php?amount=1&type=boolean&encode=base64"
    jsonResult <- httpLbs req man
    print $ B64.decode . B.toStrict . responseBody $ jsonResult
    case statusCode . responseStatus $ jsonResult of
      200 -> do
        let response = decode (responseBody jsonResult) :: Maybe FactDbResponse
        print response
        return $ fmap results response
      _ -> return Nothing
  case fetch of
    Nothing           -> activateTrivia
    Just []           -> activateTrivia
    Just (result : _) -> do
      let answer = unwrap $ correct_answer result
      messageM $ unwrap . question $ result
      u <- awaitMLoop
        [ChatCommand "True", ChatCommand "False"]
        10000
        ()
        (\case
          EventResult (ChatCommand x) (ChatCommandResult usr) ->
            if answer == x then return $ Just . Just $ usr else return $ Nothing
          EventResult Timeout TimeoutResult -> return $ Just Nothing
          _ -> return Nothing
        )
      case u of
        Nothing ->
          messageM
            $  "the correct answer was "
            <> (unwrap . correct_answer $ result)
        Just m -> messageM $ (displayName . user $ m) <> " answered correctly"
      activateTrivia

data FactDbResponse = FactDbResponse
  { response_code :: Int
  , results :: [FactResult]
  } deriving (Show, Eq, Generic)

data FactResult = FactResult
  { category :: StringTypeSynonym
  , type_ :: StringTypeSynonym
  , difficulty :: StringTypeSynonym
  , question :: StringTypeSynonym
  , correct_answer :: StringTypeSynonym
  , incorrect_answers :: [StringTypeSynonym]
  } deriving (Show, Eq, Generic)

data StringTypeSynonym = StringTypeSynonym {unwrap :: StringType } deriving (Show, Eq)

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

instance FromJSON FactResult where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = ownerFieldRename }

instance FromJSON FactDbResponse

