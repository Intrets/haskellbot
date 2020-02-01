{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Conc where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

data Conc p
  = End
  | Fork [Conc p]
         (Conc p)
  | IOtask (IO (Conc p))
  | Pure (p (Conc p))

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
(>><) list x = Fork list . x

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
      [] -> liftIO $ threadDelay 1000
      _  -> forM_ q $ \case
        End       -> liftIO $ threadDelay 1000
        IOtask io -> void . liftIO . forkIO $ do
          a <- io
          atomically $ modifyTVar queue (a :)
          liftIO $ threadDelay 1000
        Pure p -> do
          a <- p
          liftIO $ atomically $ modifyTVar queue (a :)
          liftIO $ threadDelay 1000
        Fork group next -> do
          liftIO $ atomically $ modifyTVar queue ((next : group) ++)
          liftIO $ threadDelay 1000
    go queue
