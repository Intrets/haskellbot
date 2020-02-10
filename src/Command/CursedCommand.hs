{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command.CursedCommand
  ( FactDbResponse()
  , FactResult()
  , dubiousFact
  , activateTrivia
  )
where

import Network.URI.Encode (decodeByteString)
import Data.ByteString.Lazy
import GHC.Generics
import Bot
import Command.RenameUtils
import Conc
import Data.Aeson

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import MessageQueue
import Data.Text as T
import qualified Data.List as L (head)

import Control.Monad.Reader

activateTrivia :: ConcM App ()
activateTrivia = do
  _ <- awaitM_ [ChatCommand "!df"] 0
  dubiousFactTrivia

dubiousFactTrivia :: ConcM App ()
dubiousFactTrivia = do
  man   <- pureM $ asks httpsManager
  fetch <- taskM $ do
    -- man <- newManager tlsManagerSettings
    let req = "http://opentdb.com/api.php?amount=1&type=boolean&encode=url3986"
    jsonResult <- httpLbs req man
    case statusCode . responseStatus $ jsonResult of
      200 -> do
        let
          response =
            decode
              ( fromStrict
              . decodeByteString
              . toStrict
              . responseBody
              $ jsonResult
              ) :: Maybe FactDbResponse
        return $ fmap results response
      _ -> return Nothing
  -- let fetch = Just . pure $ FactResult "" "" "" "test question" "test" ["test"]
  case fetch of
    Nothing           -> activateTrivia
    Just []           -> activateTrivia
    Just (result : _) -> do
      let answer = T.pack $ correct_answer result
      pureM $ queueMessage . T.pack . question $ result
      u <- awaitMLoop
        [ChatCommand "True", ChatCommand "False"]
        10000
        ()
        (\case
          EventResult (ChatCommand x) (ChatCommandResult user) ->
            if answer == x
              then return $ Just . Just $ user
              else return $ Nothing
          EventResult Timeout TimeoutResult -> return $ Just Nothing
          _ -> return Nothing
        )
      case u of
        Nothing ->
          pureM
            $  queueMessage
            $  "the correct answer was "
            <> (T.pack . correct_answer $ result)
        Just m ->
          pureM
            $  queueMessage
            $  (displayName . user $ m)
            <> " answered correctly"
      activateTrivia

dubiousFact :: ConcM App ()
dubiousFact = do
  cont <- taskM $ do
    man <- newManager tlsManagerSettings
    let req = "http://opentdb.com/api.php?amount=1&type=boolean"
    jsonResult <- httpLbs req man
    closeManager man
    case statusCode . responseStatus $ jsonResult of
      200 -> do
        let response = decode (responseBody jsonResult) :: Maybe FactDbResponse
        case response of
          Nothing -> return Nothing
          Just (results -> L.head -> question -> factResult) ->
            return $ Just $ pureM $ do
              queueMessage $ T.pack factResult
      _ -> return Nothing
  case cont of
    Nothing -> return ()
    Just c  -> c

data FactDbResponse = FactDbResponse
  { response_code :: Int
  , results :: [FactResult]
  } deriving (Show, Eq, Generic)

data FactResult = FactResult
  { category :: String
  , type_ :: String
  , difficulty :: String
  , question :: String
  , correct_answer :: String
  , incorrect_answers :: [String]
  } deriving (Show, Eq, Generic)

instance FromJSON FactResult where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = ownerFieldRename }
instance FromJSON FactDbResponse

