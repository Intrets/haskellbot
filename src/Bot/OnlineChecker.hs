module Bot.OnlineChecker where

import Conc
import Bot

import Network.HTTP.Client
import Network.HTTP.Simple (setRequestHeader)
import Network.HTTP.Types

import qualified Data.Text as T
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class
import Control.Category ((>>>))
import Data.Function ((&))

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

import Control.Concurrent
import Control.Monad.State.Strict

isOnline :: App Bool
isOnline = App get

onlineBlocker :: ConcM App ()
onlineBlocker = do
  (man, channel, clientID) <-
    pureM
    $   (,,)
    <$> asks httpsManager
    <*> asks (programOptions >>> ircChannel)
    <*> asks (programOptions >>> clientID)
  result <- taskM $ do
    let
      req =
        T.unpack
            ("https://api.twitch.tv/helix/streams?user_login=" <> T.tail channel
            )
          & parseRequest_
          & setRequestHeader "Client-ID" [B.pack . T.unpack $ clientID]
    print req
    x <- httpLbs req man
    print $ length . LB.unpack $ responseBody x
    return x
  let
    online = case statusCode . responseStatus $ result of
      200 -> case length . LB.unpack . responseBody $ result of
        27 -> False
        _  -> True
      _ -> True
  pureM $ App $ put online
  taskM $ threadDelay 120000000
  onlineBlocker
