module MessageQueue where

import Bot
import Bot.Irc.Send
import Conc
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Time.Clock.POSIX
import Queue

messageLift :: StateT MessageQueue IO a -> App a
messageLift = App . lift . lift . lift . lift

queueMessage :: StringType -> App ()
queueMessage message = messageLift
  $ modify (\(MessageQueue time q) -> MessageQueue time (push message q))

messageDispensingLoopM :: ConcM App ()
messageDispensingLoopM = do
  _ <- pureM $ do
    MessageQueue time q <- messageLift get
    if isEmpty q
      then return ()
      else do
        currentTime <- liftIO getPOSIXTime
        when (currentTime > time) $ do
          let (Just message, q_new) = pop q
          messageLift . put $ MessageQueue (currentTime + 2) q_new
          privmsg message
          liftIO $ print $ currentTime > time
  _ <- taskM $ threadDelay 100000
  messageDispensingLoopM

