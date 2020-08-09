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
import qualified Data.Text as T

messageDispensingLoopM2 :: ConcM App ()
messageDispensingLoopM2 = do
  taskM $ threadDelay 2000000
  (msgQ   , online) <- pureM $ (,) <$> asks messageQueue <*> isOnline
  (message, sem   ) <- taskM $ atomically $ readTBQueue msgQ
  let s = T.chunksOf 130 . T.take 260 $ message
  forM_ (init s) $ \m ->
    (pureM $ privmsg m) >> (taskM $ threadDelay 2000000)
--  pureM $ do
--    when (T.length message < 500) $ privmsg message
--    --unless online $ privmsg message
  pureM $ privmsg $ last s
  taskM $ putMVar sem True
  forkM [messageDispensingLoopM2]
