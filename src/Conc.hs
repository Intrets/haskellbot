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
  end :: (a -> m b) -> (a -> Conc n)
  foreverC :: (a -> m a) -> (a -> Conc n)
  foreverC_ :: (() -> m ()) -> Conc n

instance Composable n IO where
  (>>.) x y a = IOtask $ do x a >> return y
  end x a = IOtask $ do x a >> return End
  foreverC x = x >>- foreverC x
  foreverC_ x = foreverC x ()

instance (Monad n) => Composable n n where
  (>>.) x y a = Pure $ do x a >> return y
  end x a = Pure $ do x a >> return End
  foreverC x = x >>+ foreverC x
  foreverC_ x = foreverC x ()

startPure :: (Monad n) => n b -> (b -> Conc n) -> Conc n
startPure x y = Pure $ do x >>= return . y

startIO :: IO b -> (b -> Conc n) -> Conc n
startIO x y = IOtask $ do x >>= return . y

(>><) ::[Conc n] -> (b -> Conc n) -> (b -> Conc n)
(>><) list x = (Fork list) . x

infixr 2 >><

-- (Conc n -> Conc n) -> (b -> Conc n) -> (b -> Conc n)
(>>-) :: (a -> IO b) -> (b -> Conc n) -> (a -> Conc n)
(>>-) x y a = IOtask $ do x a >>= return . y

(>>+) :: (Monad n) => (a -> n b) -> (b -> Conc n) -> (a -> Conc n)
(>>+) x y a = Pure $ do x a >>= return . y

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
        [] -> liftIO $ threadDelay 10
        _ -> do
          forM_ q $ \case
            End -> return ()
            IOtask io -> do
              void . liftIO . forkIO $ do
                a <- io
                atomically $ modifyTVar queue (a :)
            Pure p -> do
              a <- p
              liftIO $ atomically $ modifyTVar queue (a :)
            Fork group next -> do
              liftIO $ atomically $ modifyTVar queue ((next : group) ++ )
      go queue