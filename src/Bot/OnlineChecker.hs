module Bot.OnlineChecker where

import Conc
import Bot

import Network.HTTP.Client
import Network.HTTP.Simple (addRequestHeader)
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
  (man, channel, clientID, auth, ircNick) <-
    pureM
    $   (,,,,)
    <$> asks httpsManager
    <*> asks (programOptions >>> ircChannel)
    <*> asks (programOptions >>> clientID)
    <*> asks (programOptions >>> ircOauth)
    <*> asks (programOptions >>> ircNick)
  result <- taskM $ do
    let
      chan = T.tail channel
      req =
        T.unpack ("https://api.twitch.tv/helix/streams?user_login=" <> chan)
          & parseRequest_
          & addRequestHeader "Authorization" (B.pack . T.unpack $ ("iBearer " <> auth))
          & addRequestHeader "Client-ID" (B.pack . T.unpack $ clientID)
    httpLbs req man <* print req
  let
    online = case statusCode . responseStatus $ result of
      200 -> case length . LB.unpack . responseBody $ result of
        27 -> False
        _  -> True
      _ -> True
  taskM $ print result 
  pureM $ App $ put online
  taskM $ threadDelay 120000000
  onlineBlocker
