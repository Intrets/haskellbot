{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Command.CursedCommand
  ( FactDbResponse()
  , FactResult()
  , dubiousFact
  )
where

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

dubiousFact :: ConcM App ()
dubiousFact = do
  cont <- taskM $ do
    man <- newManager tlsManagerSettings
    let req = "http://opentdb.com/api.php?amount=1&type=boolean"
    jsonResult <- httpLbs req man
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

