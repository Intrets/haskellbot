module Bot.OnlineChecker where

import           Conc
import           Bot
import           Network.HTTP.Client
import           Network.HTTP.Simple (addRequestHeader)
import           Network.HTTP.Types
import qualified Data.Text as T
import           Control.Monad.Reader (asks)
import           Control.Category ((>>>))
import           Data.Function ((&))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Control.Concurrent
import           Control.Monad.State.Strict

isOnline :: App Bool
isOnline = App get

onlineBlocker :: ConcM App ()
onlineBlocker = do
  (man, channel, client_id, auth) <- pureM
    $ (,,,) <$> asks httpsManager
    <*> asks (programOptions >>> ircChannel)
    <*> asks (programOptions >>> clientID)
    <*> asks (programOptions >>> helixOauth)
  result <- taskM
    $ do
      let chan = T.tail channel
          req = T.unpack
            ("https://api.twitch.tv/helix/streams?user_login=" <> chan)
            & parseRequest_
            & addRequestHeader
              "Authorization"
              (B.pack . T.unpack $ ("Bearer " <> auth))
            & addRequestHeader "Client-ID" (B.pack . T.unpack $ client_id)
      httpLbs req man
  let online = case statusCode . responseStatus $ result of
        200 -> case length . LB.unpack . responseBody $ result of
          27 -> False
          _  -> True
        _   -> True
  taskM $ print $ "putting bot " ++ if online then "offline" else "online"
  pureM $ App $ put online
  taskM $ threadDelay 120000000
  onlineBlocker
