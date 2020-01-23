module Bot.Random where

import Bot

import Control.Monad.State.Strict
import System.Random

stateTest :: (MonadState Int m) => m Int 
stateTest = do
  c <- get
  put . succ $ c
  return c

randRange :: (Random a, Num a, Ord a, RandomGenerator m) => a -> a -> m a
randRange low high
  | low > high = pure low
  | otherwise = state $ randomR (low, high)

dicegolf :: (MonadState StdGen m) => Int -> m [Int]
dicegolf 1 = return [1]
dicegolf d = do
  roll <- randRange 1 d
  n <- dicegolf roll
  return (d : n)
