module Bot.Random where

import Bot

import Control.Monad.State.Strict
import System.Random
import qualified Data.Array as A

class (Monad m) =>
      RandomGenerator m where
  randRange :: (Num a, Ord a, Random a) => a -> a -> m a
  pick :: (A.Ix a, Num a, Ord a, Random a) => A.Array a b -> m b

instance RandomGenerator App where
  randRange low high
    | low > high = App $ pure low
    | otherwise = App . lift . lift $ (state $ randomR (low, high))
  pick array = do
    let range = A.bounds array
    i <- uncurry randRange range
    return $ array A.! i

dicegolf :: (RandomGenerator m) => Int -> m [Int]
dicegolf 1 = return [1]
dicegolf d = do
  roll <- randRange 1 d
  n <- dicegolf roll
  return (d : n)
