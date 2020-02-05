{-# LANGUAGE Arrows #-}

module MessageQueue where

import Bot
import Bot.Irc.Send
import Conc
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Function ((&))
import Data.Time.Clock.POSIX
import Queue
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad.Reader
import GHC.IO.Handle (Handle)
import Control.Arrow

messageLift :: StateT MessageQueue IO a -> App a
messageLift = App . lift . lift . lift . lift . lift

queueMessage :: StringType -> App ()
queueMessage message = messageLift
  $ modify (\(MessageQueue time q) -> MessageQueue time (push message q))

messageDispensingLoopM :: ConcM App ()
messageDispensingLoopM = do
  _ <- pureM $ do
    MessageQueue time queue <- messageLift get
    if isEmpty queue
      then return ()
      else do
        currentTime <- liftIO getPOSIXTime
        when (currentTime > time) $ do
          let (Just message, q) = pop queue
          messageLift . put $ MessageQueue (currentTime + 2) q
          privmsg message
          liftIO $ print $ currentTime > time
  _ <- taskM $ threadDelay 100000
  messageDispensingLoopM

