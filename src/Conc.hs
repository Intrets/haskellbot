{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}


module Conc where
import Bot
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad
import Control.Monad.IO.Class
import Data.Text as T
import Control.Monad.State
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics
import Data.Maybe (catMaybes)
import Control.Monad.Reader

data Wat = Int

data Event = ChatCommand T.Text | ChatWord T.Text | Timeout deriving (Generic, Eq)
--data EventResult = ChatCommandResult Event User | ChatWordResult Event User deriving (Generic, Eq)

instance Hashable Event

data EventResultType = ChatCommandResult Message | ChatWordResult Message | TimeoutResult

data EventResult = EventResult Event EventResultType

data Action m b
  = ActionIO (IO b)
  | ActionPure (m b)
  | ActionAwait [Event] Int (EventResult -> b)
  | forall s. ActionAwaitLoop [Event] Int s (EventResult -> State s (Maybe b))
  | ActionSend EventResult b
  | ActionMessage StringType b

awaitMLoop
  :: [Event] -> Int -> s -> (EventResult -> State s (Maybe a)) -> ConcM m a
awaitMLoop events timout s actions =
  Task (ActionAwaitLoop events timout s actions) EndM

awaitM :: [Event] -> Int -> (EventResult -> a) -> ConcM m a
awaitM events timeout action = Task (ActionAwait events timeout action) EndM

awaitM_ :: [Event] -> Int -> ConcM m ()
awaitM_ events timeout = Task (ActionAwait events timeout (const ())) EndM

sendM :: Event -> EventResultType -> ConcM App ()
sendM event result = Task (ActionSend (EventResult event result) ()) EndM

data ConcM m c
  = EndM c
  | ForkM [ConcM m ()] (ConcM m c)
  | forall b. Task (Action m b) (b -> ConcM m c)

instance (Monad m) =>  Functor (ConcM m) where
  fmap f (EndM c           ) = EndM $ f c
  fmap f (ForkM forks  cont) = ForkM forks (fmap f cont)
  fmap f (Task  action cont) = Task action (fmap f . cont)

instance  (Monad m) => Applicative (ConcM m) where
  pure = EndM
  (EndM f           ) <*> t = fmap f t
  (ForkM forks  cont) <*> t = ForkM forks (cont <*> t)
  (Task  action cont) <*> t = Task action ((<*> t) . cont)

instance (Monad m) => Monad (ConcM m) where
  (EndM a            ) >>= cont  = Task (ActionPure (pure a)) cont
  (ForkM forks  cont1) >>= cont2 = ForkM forks (cont1 >>= cont2)
  (Task  action cont1) >>= cont2 = Task action ((>>= cont2) . cont1)

forkM :: [ConcM m ()] -> ConcM m ()
forkM forks = ForkM forks (EndM ())

pureM :: m c -> ConcM m c
pureM x = Task (ActionPure x) EndM

taskM :: IO c -> ConcM m c
taskM x = Task (ActionIO x) EndM

messageM :: StringType -> ConcM m ()
messageM msg = Task (ActionMessage msg ()) EndM

getChan
  :: [Event]
  -> M.HashMap Event (TChan EventResult)
  -> IO ([TChan EventResult], M.HashMap Event (TChan EventResult))
getChan es = runStateT $ do
  s <- get
  forM es $ \e -> case M.lookup e s of
    Nothing -> do
      new <- liftIO . atomically $ newBroadcastTChan
      modify (M.insert e new)
      liftIO $ atomically $ dupTChan new
    Just r -> liftIO . atomically $ dupTChan r

getTimeOutChan :: Int -> IO (Maybe (TChan EventResult))
getTimeOutChan 0       = return Nothing
getTimeOutChan timeout = do
  timeoutChan <- newBroadcastTChanIO
  void . forkIO $ do
    threadDelay $ timeout * 1000
    atomically $ writeTChan timeoutChan (EventResult Timeout TimeoutResult)
  Just <$> (atomically . dupTChan $ timeoutChan)

subscribe
  :: [Event]
  -> TVar (M.HashMap Event (TChan EventResult))
  -> Int
  -> IO (IO EventResult)
subscribe events channels timeout = do
  timeoutChan              <- getTimeOutChan timeout
  chans                    <- readTVarIO channels
  (listenChannels, chans2) <- getChan events chans
  atomically $ writeTVar channels chans2
  return $ atomically $ Prelude.foldl1
    orElse
    (Prelude.map readTChan (catMaybes [timeoutChan] ++ listenChannels))

sendEvent :: TVar (M.HashMap Event (TChan EventResult)) -> EventResult -> IO ()
sendEvent channels r@(EventResult event _) = do
  chans <- readTVarIO channels
  case M.lookup event chans of
    Nothing -> return ()
    Just c  -> atomically $ writeTChan c r

runConcM :: (Monad a, MonadIO a, MonadReader Options a) => [ConcM a ()] -> a ()
runConcM q_ = do
  queue' <- liftIO . atomically $ do
    q <- newTQueue
    mapM_ (writeTQueue q) q_
    return q
  channels <-
    liftIO . newTVarIO $ (M.empty :: M.HashMap Event (TChan EventResult))
  _ <- forever $ go queue' channels
  return ()
 where
  go queue' channels = do
    q <- liftIO . atomically $ readTQueue queue'
    case q of
      EndM _ -> return ()
      ForkM forks cont ->
        liftIO . atomically $ mapM_ (writeTQueue queue') (cont : forks)
      Task (ActionIO io) cont -> void . liftIO . forkIO $ do
        a <- io
        atomically $ writeTQueue queue' (cont a)
      Task (ActionPure p) cont -> do
        a <- p
        liftIO $ atomically $ writeTQueue queue' (cont a)
      Task (ActionAwaitLoop events timeout s action) cont ->
        void . liftIO . forkIO $ do
          sub <- subscribe events channels timeout
          a   <- listen sub s action
          liftIO $ atomically $ writeTQueue queue' (cont a)
      Task (ActionAwait events timeout action) cont ->
        void . liftIO . forkIO $ do
          sub <- subscribe events channels timeout
          a   <- sub
          liftIO $ atomically $ writeTQueue queue' (cont . action $ a)
      Task (ActionSend event b) cont -> do
        liftIO $ sendEvent channels event
        liftIO $ atomically $ writeTQueue queue' (cont b)
      Task (ActionMessage txt b) cont -> do
        msgQ <- asks messageQueue
        sem  <- liftIO newEmptyMVar
        void . liftIO . forkIO $ do
          atomically $ writeTBQueue msgQ (txt, sem)
          _ <- takeMVar sem
          atomically $ writeTQueue queue' (cont b)

listen :: IO EventResult -> s -> (EventResult -> State s (Maybe b)) -> IO b
listen h s events = do
  e <- h
  let (continue, s2) = runState (events e) s
  case continue of
    Nothing -> listen h s2 events
    Just b  -> return b








