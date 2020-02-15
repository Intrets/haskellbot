module MessageQueue where

import Bot
import Bot.Irc.Send
import Conc
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Time.Clock.POSIX
import Queue
import Control.Monad.Reader
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM
import Bot.OnlineChecker

messageDispensingLoopM2 :: ConcM App ()
messageDispensingLoopM2 = do
  (msgQ   , online) <- pureM $ (,) <$> asks messageQueue <*> isOnline
  (message, sem   ) <- taskM $ atomically $ readTBQueue msgQ
  pureM $ do
    liftIO $ print $ "channel online: " ++ show online
    unless online $ privmsg message
    liftIO $ putMVar sem True
  taskM $ threadDelay 2000000
  messageDispensingLoopM2
