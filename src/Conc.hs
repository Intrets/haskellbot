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

instance (Monad m) => C.Category (Conc2 m) where
  id = Id2 id
  cont              . (IOtask2 io cont1) = IOtask2 io (cont C.. cont1)
  cont              . (Pure2   p  cont1) = Pure2 p (cont C.. cont1)
  (IOtask2 io cont) . Id2 f              = IOtask2 (io . f) cont
  (Pure2   p  cont) . Id2 f              = Pure2 (p . f) cont
  Id2 g             . Id2 f              = Id2 (g . f)

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

class Composable n m where
  (>>.) :: (a -> m b) -> Conc n -> (a -> Conc n)
  foreverC :: (a -> m a) -> (a -> Conc n)
  foreverC_ :: (() -> m ()) -> Conc n

instance Composable n IO where
  (>>.) x y a = IOtask $ x a >> return y
  foreverC x = x >>- foreverC x
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
(>>-) :: (a -> IO b) -> (b -> Conc n) -> (a -> Conc n)
(>>-) x y a = IOtask $ y <$> x a

(>>+) :: (Monad n) => (a -> n b) -> (b -> Conc n) -> (a -> Conc n)
(>>+) x y a = Pure $ y <$> x a

infixr 2 >>-

infixr 2 >>+

infixr 2 >>.

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
