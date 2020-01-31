{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.CursedCommand
  ( FactDbResponse()
  , FactResult()
  , dubiousFact
  )
where

import Bot
import Command.RenameUtils
import Conc
import Data.Aeson
import Data.Aeson.TH

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import MessageQueue
import Data.Text as T
import qualified Data.List as L (head)

dubiousFact :: Conc App
dubiousFact = IOtask $ do
  man <- newManager tlsManagerSettings
  let req = "http://opentdb.com/api.php?amount=1&type=boolean"
  jsonResult <- httpLbs req man
  case statusCode . responseStatus $ jsonResult of
    200 -> do
      let response = decode (responseBody jsonResult) :: Maybe FactDbResponse
      case response of
        Nothing -> return End
        Just (results -> L.head -> question -> factResult) ->
          return $ Pure $ do
            queueMessage $ T.pack factResult
            return End
    _ -> return End

test = 1

data FactDbResponse = FactDbResponse
  { response_code :: Int
  , results :: [FactResult]
  } deriving (Show, Eq)

data FactResult = FactResult
  { category :: String
  , type_ :: String
  , difficulty :: String
  , question :: String
  , correct_answer :: String
  , incorrect_answers :: [String]
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''FactDbResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = ownerFieldRename} ''FactResult)
