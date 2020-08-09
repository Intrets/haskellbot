module Bot.Random where

import Bot

import Control.Monad.State.Strict
import System.Random
import qualified Data.Array as A
import Data.Array.ST
import GHC.Arr

class (Monad m) =>
      RandomGenerator m where
  randRange :: (Num a, Ord a, Random a) => a -> a -> m a
  pick :: (A.Ix a, Num a, Ord a, Random a) => A.Array a b -> m b
  permutation :: [a] -> m [a]

instance RandomGenerator App where
  randRange low high
    | low > high = App $ pure low
    | otherwise  = App . lift . lift $ state (randomR (low, high))
  pick array = do
    let range = A.bounds array
    i <- uncurry randRange range
    return $ array A.! i
  permutation xs = do
    let l = length xs
    rands <- forM [0 .. (l - 2)] (flip randRange (l - 1))
    let
      ar = runSTArray $ do
        ar <- thawSTArray $ A.listArray (0, l - 1) xs
        forM_ (zip [0 ..] rands) $ \(i, j) -> do
          vi <- readSTArray ar i
          vj <- readSTArray ar j
          writeSTArray ar j vi
          writeSTArray ar i vj
        return ar
    return (A.elems ar)

dicegolf :: (RandomGenerator m) => Integer -> m [Integer]
dicegolf 1 = return [1]
dicegolf d = do
  roll <- randRange 1 d
  n    <- dicegolf roll
  return (d : n)

