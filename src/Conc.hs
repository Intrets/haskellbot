{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Conc where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Category as C
import Control.Arrow

data ConcM m c
  = EndM c
  | forall b. IOtaskM (IO b) (b -> ConcM m c)
  | forall b. PureM (m b) (b -> ConcM m c)

instance (Monad m) => Functor (ConcM m) where
  fmap f (EndM c         ) = EndM $ f c
  fmap f (IOtaskM io cont) = IOtaskM io (fmap f . cont)
  fmap f (PureM   p  cont) = PureM p (fmap f . cont)

instance (Monad m) => Applicative (ConcM m) where
  pure = EndM
  t                 <*> (IOtaskM io cont) = IOtaskM io (\c -> t <*> cont c)
  t                 <*> (PureM   p  cont) = PureM p (\c -> t <*> cont c)
  (EndM f         ) <*> (EndM c         ) = EndM (f c)
  (IOtaskM io cont) <*> (EndM c) = IOtaskM io (\c2 -> cont c2 <*> EndM c)
  (PureM   p  cont) <*> (EndM c         ) = PureM p (\c2 -> cont c2 <*> EndM c)

instance (Monad m) => Monad (ConcM m) where
  (EndM a          ) >>= cont  = PureM (pure a) cont
  (IOtaskM io cont1) >>= cont2 = IOtaskM io (\a -> cont1 a >>= cont2)
  (PureM   p  cont1) >>= cont2 = PureM p (\a -> cont1 a >>= cont2)

pureM :: m c -> ConcM m c
pureM x = PureM x EndM

taskM :: IO c -> ConcM m c
taskM x = IOtaskM x EndM

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
        EndM _          -> return ()
        IOtaskM io cont -> void . liftIO . forkIO $ do
          a <- io
          atomically $ modifyTVar queue (cont a :)
        PureM p cont -> do
          a <- p
          liftIO $ atomically $ modifyTVar queue (cont a :)
    go queue

