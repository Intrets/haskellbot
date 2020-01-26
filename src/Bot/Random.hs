module Bot.Random where

import Bot

import Control.Monad.State.Strict
import System.Random

class (Monad m) =>
      RandomGenerator m where
  randRange :: (Num a, Ord a, Random a) => a -> a -> m a

instance RandomGenerator App where
  randRange low high
    | low > high = App $ pure low
    | otherwise = App . lift . lift $ (state $ randomR (low, high))

dicegolf :: (RandomGenerator m) => Int -> m [Int]
dicegolf 1 = return [1]
dicegolf d = do
  roll <- randRange 1 d
  n <- dicegolf roll
  return (d : n)
