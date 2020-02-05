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

data Conc p
  = End
  | Fork [Conc p]
         (Conc p)
  | IOtask (IO (Conc p))
  | Pure (p (Conc p))

data Conc2 p a c
  = Id2 (a -> c)
  | forall b. IOtask2 (a -> IO b) (Conc2 p b c)
  | forall b. Pure2 (a -> p b) (Conc2 p b c)

data RunnableConc2 p c
  = RId2 c
  | forall b. RIOtask2 (IO b) (Conc2 p b c)
  | forall b. RPure2 (p b) (Conc2 p b c)

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
  -- liftA2 f (IOtaskM io cont) (IOtaskM io cont) =  
  -- (IOtaskM io1 cont1)  <*> (IOtaskM io2  cont2) =  IOtaskM io2 (IOtasmM io1 (cont1 <*> cont2))
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

makeRunConc2 :: a -> Conc2 p a c -> RunnableConc2 p c
makeRunConc2 a (Id2 f          ) = RId2 (f a)
makeRunConc2 a (IOtask2 io cont) = RIOtask2 (io a) cont
makeRunConc2 a (Pure2   p  cont) = RPure2 (p a) cont

instance (Monad m) => C.Category (Conc2 m) where
  id = Id2 id
  cont              . (IOtask2 io cont1) = IOtask2 io (cont C.. cont1)
  cont              . (Pure2   p  cont1) = Pure2 p (cont C.. cont1)
  (IOtask2 io cont) . Id2 f              = IOtask2 (io . f) cont
  (Pure2   p  cont) . Id2 f              = Pure2 (p . f) cont
  Id2 g             . Id2 f              = Id2 (g . f)

cPure :: (a -> p c) -> Conc2 p a c
cPure p = Pure2 p (Id2 id)

cIOtask :: (a -> IO c) -> Conc2 p a c
cIOtask io = IOtask2 io (Id2 id)

instance (Monad m) => Arrow (Conc2 m) where
  arr = Id2
  first (Pure2 p cont) = Pure2
    (\(b, d) -> do
      res <- p b
      return (res, d)
    )
    (first cont)
  first (IOtask2 io cont) = IOtask2
    (\(b, d) -> do
      res <- io b
      return (res, d)
    )
    (first cont)
  first (Id2 f) = Id2 (first f)

instance (Monad m) => ArrowChoice (Conc2 m) where
  left (Id2 f       ) = Id2 (left f)
  left (Pure2 p cont) = Pure2
    (\case
      Left b -> do
        res <- p b
        return $ Left res
      Right d -> return $ Right d
    )
    (left cont)
  left (IOtask2 io cont) = IOtask2
    (\case
      Left b -> do
        res <- io b
        return $ Left res
      Right d -> return $ Right d
    )
    (left cont)


class Composable n m where
  (>>.) :: (a -> m b) -> Conc n -> (a -> Conc n)
  foreverC :: (a -> m a) -> (a -> Conc n)
  foreverC_ :: (() -> m ()) -> Conc n

instance Composable n IO where
  (>>.) x y a = IOtask $ x a >> return y
  foreverC x = x >>-- foreverC x
  foreverC_ x = foreverC x ()

endIO :: IO a -> Conc n
endIO x = IOtask $ x >> return End

instance (Monad n) => Composable n n where
  (>>.) x y a = Pure $ x a >> return y
  foreverC x = x >>+ foreverC x
  foreverC_ x = foreverC x ()

end :: Monad n => n a -> Conc n
end x = Pure $ x >> return End

startPure :: (Monad n) => n b -> (b -> Conc n) -> Conc n
startPure x y = Pure $ y <$> x

startIO :: IO b -> (b -> Conc n) -> Conc n
startIO x y = IOtask $ y <$> x

(>><) :: [Conc n] -> (b -> Conc n) -> (b -> Conc n)
(>><) list x = Fork list Prelude.. x

infixr 2 >><

-- (Conc n -> Conc n) -> (b -> Conc n) -> (b -> Conc n)
(>>--) :: (a -> IO b) -> (b -> Conc n) -> (a -> Conc n)
(>>--) x y a = IOtask $ y <$> x a

(>>+) :: (Monad n) => (a -> n b) -> (b -> Conc n) -> (a -> Conc n)
(>>+) x y a = Pure $ y <$> x a

infixr 2 >>--

infixr 2 >>+

infixr 2 >>.

runConc2 :: (Monad m, MonadIO m) => [RunnableConc2 m b] -> m ()
runConc2 q_ = do
  queue <- liftIO $ atomically $ newTVar q_
  go queue
 where
  go queue = do
    q <- liftIO $ atomically $ readTVar queue <* writeTVar queue []
    case q of
      [] -> liftIO $ threadDelay 100000
      _  -> forM_ q $ \case
        RId2 _           -> return ()
        RIOtask2 io cont -> void . liftIO . forkIO $ do
          b <- io
          atomically $ modifyTVar queue (makeRunConc2 b cont :)
        RPure2 p cont -> do
          b <- p
          liftIO $ atomically $ modifyTVar queue (makeRunConc2 b cont :)
    go queue

runConc :: (Monad a, MonadIO a) => [Conc a] -> a ()
runConc q_ = do
  queue <- liftIO $ atomically $ newTVar q_
  go queue
 where
  go queue = do
    q <- liftIO $ atomically $ readTVar queue <* writeTVar queue []
    case q of
      [] -> liftIO $ threadDelay 100000
      _  -> forM_ q $ \case
        End       -> return ()
        IOtask io -> void . liftIO . forkIO $ do
          a <- io
          atomically $ modifyTVar queue (a :)
        Pure p -> do
          a <- p
          liftIO $ atomically $ modifyTVar queue (a :)
        Fork group next ->
          liftIO $ atomically $ modifyTVar queue ((next : group) ++)
    go queue
