{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Conc where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

data ConcM m c
  = EndM c
  | ForkM [ConcM m ()] (ConcM m c)
  | forall b. TaskM (IO b) (b -> ConcM m c)
  | forall b. PureM (m b) (b -> ConcM m c)

instance (Monad m) => Functor (ConcM m) where
  fmap f (EndM c          ) = EndM $ f c
  fmap f (ForkM forks cont) = ForkM forks (fmap f cont)
  fmap f (TaskM io    cont) = TaskM io (fmap f . cont)
  fmap f (PureM p     cont) = PureM p (fmap f . cont)

instance (Monad m) => Applicative (ConcM m) where
  pure = EndM
  -- t               <*> (TaskM io cont) = TaskM io ((t <*>) . cont)
  -- t               <*> (PureM p  cont) = PureM p ((t <*>) . cont)
  -- (EndM f       ) <*> (EndM c       ) = EndM (f c)
  -- (TaskM io cont) <*> (EndM c       ) = TaskM io ((<*> EndM c) . cont)
  -- (PureM p  cont) <*> (EndM c       ) = PureM p (\c2 -> cont c2 <*> EndM c)
  (EndM f          ) <*> t = fmap f t
  (ForkM forks cont) <*> t = ForkM forks (cont <*> t)
  (TaskM io    cont) <*> t = TaskM io ((<*> t) . cont)
  (PureM p     cont) <*> t = PureM p ((<*> t) . cont)

instance (Monad m) => Monad (ConcM m) where
  (EndM a           ) >>= cont  = PureM (pure a) cont
  (ForkM forks cont1) >>= cont2 = ForkM forks (cont1 >>= cont2)
  (TaskM io    cont1) >>= cont2 = TaskM io ((>>= cont2) . cont1)
  (PureM p     cont1) >>= cont2 = PureM p ((>>= cont2) . cont1)

forkM :: [ConcM m ()] -> ConcM m ()
forkM forks = ForkM forks (EndM ())

pureM :: m c -> ConcM m c
pureM x = PureM x EndM

taskM :: IO c -> ConcM m c
taskM x = TaskM x EndM

runConcM :: (Monad a, MonadIO a) => [ConcM a ()] -> a ()
runConcM q_ = do
  queue <- liftIO $ atomically $ newTVar q_
  go queue
 where
  go queue = do
    q <- liftIO $ atomically $ readTVar queue <* writeTVar queue []
    case q of
      [] -> liftIO $ threadDelay 100000
      _  -> forM_ q $ \case
        EndM _ -> return ()
        ForkM forks cont ->
          liftIO $ atomically $ modifyTVar queue ((cont : forks) ++)
        TaskM io cont -> void . liftIO . forkIO $ do
          a <- io
          atomically $ modifyTVar queue (cont a :)
        PureM p cont -> do
          a <- p
          liftIO $ atomically $ modifyTVar queue (cont a :)
    go queue

