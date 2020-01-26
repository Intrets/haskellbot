module MessageQueue where

import Bot
import Bot.Irc.Send
import Conc
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Function ((&))
import Data.Time.Clock.POSIX
import Queue

messageLift = App . lift . lift . lift . lift . lift

queueMessage :: StringType -> App ()
queueMessage message = do
  messageLift $
    modify (\(MessageQueue time q) -> MessageQueue time (push message q))

messageDispensingLoop :: Conc App
messageDispensingLoop =
  1 &
  (const $ do
     MessageQueue time queue <- messageLift get
     case isEmpty queue of
       True -> return ()
       False -> do
         currentTime <- liftIO $ getPOSIXTime
         when (currentTime > time) $ do
           let (Just message, q) = pop queue
           messageLift . put $ MessageQueue (currentTime + 2) q
           privmsg message
           liftIO $ print $ currentTime > time) >>+
  (const $ threadDelay 1000) >>.
  messageDispensingLoop
