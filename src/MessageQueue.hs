{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module MessageQueue where

import           Bot
import           Bot.Irc.Send
import           Conc
import           Control.Concurrent
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM
import           Bot.OnlineChecker
import qualified Data.Text as T

messageDispensingLoopM2 :: ConcM App ()
messageDispensingLoopM2 = do
  taskM $ threadDelay 2000000
  (msgQ, online, ircMessageLimit, totalMessageLimit) <- pureM
    $ (,,,) <$> asks messageQueue
    <*> isOnline
    <*> asks (ircMessageLimit . programOptions)
    <*> asks (totalMessageLimit . programOptions)
  (message, sem) <- taskM $ atomically $ readTBQueue msgQ
  --   let s = T.chunksOf 130 . T.take 260 $ message
  let (s, remain) =
        accumulateMessages (T.words message) ircMessageLimit totalMessageLimit
  let s2 =
        if null remain
        then s
        else let (T.unwords . reverse . T.words . head -> s3, _) =
                   accumulateMessages (reverse remain) (ircMessageLimit - 4) 1
             in (init s ++ ["... " <> s3])
  when online $ taskM $ print "channel online"
  unless online
    $ do
      forM_ (init s2) $ \m -> pureM (privmsg m) >> taskM (threadDelay 2000000)
      pureM $ privmsg $ last s2
  taskM $ putMVar sem True
  forkM [messageDispensingLoopM2]

accumulateMessages
  :: [StringType] -> Int -> Int -> ([StringType], [StringType])
accumulateMessages [] _ _ = ([], [])
accumulateMessages words messageLimit totalLimit
  | totalLimit <= 0 = ([], words)
  | otherwise =
    let ((T.unwords -> message), words2) = accumulateWords words messageLimit
    in if T.length message == 0
       then ([], words2)
       else let (a, b) =
                  accumulateMessages words2 messageLimit (totalLimit - 1)
            in (message:a, b)

accumulateWords :: [StringType] -> Int -> ([StringType], [StringType])
accumulateWords [] _ = ([], [])
accumulateWords (word:words) limit
  | limit - (1 + T.length word) <= 0 = ([], words)
  | otherwise =
    let (a, b) = accumulateWords words (limit - (1 + T.length word))
    in (word:a, b)






