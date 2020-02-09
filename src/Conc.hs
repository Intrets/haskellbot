{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}


module Conc where
import Bot (User(..))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Data.Text as T
import Control.Monad.State
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics

data Wat = Int

data Event = ChatCommand T.Text | ChatWord T.Text deriving (Generic, Eq)
--data EventResult = ChatCommandResult Event User | ChatWordResult Event User deriving (Generic, Eq)

data EventResultType = ChatCommandResult User | ChatWordResult User

data EventResult = EventResult Event EventResultType

instance Hashable Event

data Action m b
  = ActionIO (IO b)
  | ActionPure (m b)
  | ActionAwait [Event] (EventResult -> b)
  | forall s. ActionAwaitLoop [Event] s (EventResult -> State s (Maybe b))
  | ActionSend EventResult b

awaitMLoop :: [Event] -> s -> (EventResult -> State s (Maybe a)) -> ConcM m a
awaitMLoop events s actions = Task (ActionAwaitLoop events s actions) EndM

awaitM :: [Event] -> (EventResult -> a) -> ConcM m a
awaitM events action = Task (ActionAwait events action) EndM

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

subscribe
  :: [Event] -> TVar (M.HashMap Event (TChan EventResult)) -> IO EventResult
subscribe events channels = do
  chans                    <- readTVarIO channels
  (listenChannels, chans2) <- getChan events chans
  atomically $ writeTVar channels chans2
  atomically $ Prelude.foldl1 orElse (Prelude.map readTChan listenChannels)

sendEvent :: TVar (M.HashMap Event (TChan EventResult)) -> EventResult -> IO ()
sendEvent channels r@(EventResult event _) = do
  chans <- readTVarIO channels
  case M.lookup event chans of
    Nothing -> return ()
    Just c  -> atomically $ writeTChan c r


-- unsubscribe :: TChan Event -> IO ()
unsubscribe = undefined


runConcM :: (Monad a, MonadIO a) => [ConcM a ()] -> a ()
runConcM q_ = do
  queue <- liftIO . atomically $ do
    q <- newTQueue
    mapM_ (writeTQueue q) q_
    return q
  channels <-
    liftIO . newTVarIO $ (M.empty :: M.HashMap Event (TChan EventResult))
  _ <- forever $ go queue channels
  return ()
 where
  go queue channels = do
    q <- liftIO . atomically $ readTQueue queue
    case q of
      EndM _ -> return ()
      ForkM forks cont ->
        liftIO . atomically $ mapM_ (writeTQueue queue) (cont : forks)
      Task (ActionIO io) cont -> void . liftIO . forkIO $ do
        a <- io
        atomically $ writeTQueue queue (cont a)
      Task (ActionPure p) cont -> do
        a <- p
        liftIO $ atomically $ writeTQueue queue (cont a)
      Task (ActionAwaitLoop events s action) cont ->
        void . liftIO . forkIO $ do
          a <- listen (subscribe events channels) s action
          liftIO $ atomically $ writeTQueue queue (cont a)
      Task (ActionAwait events action) cont -> void . liftIO . forkIO $ do
        a <- subscribe events channels
        liftIO $ atomically $ writeTQueue queue (cont . action $ a)
      Task (ActionSend event b) cont -> do
        liftIO $ sendEvent channels event
        liftIO $ atomically $ writeTQueue queue (cont b)

listen :: IO EventResult -> s -> (EventResult -> State s (Maybe b)) -> IO b
listen h s events = do
  e <- h
  let (continue, s2) = runState (events e) s
  case continue of
    Nothing -> listen h s2 events
    Just b  -> return b








