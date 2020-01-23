module Bot.Catfacts where

import Bot
import Bot.Random

import Control.Monad.Reader
import qualified Data.Array as A
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

loadFacts :: String -> IO (A.Array Int String)
loadFacts path = do
  file <- openFile path ReadMode
  facts <- filter (not . null) . lines <$> hGetContents file
  hClose file
  return . A.listArray (0, pred . length $ facts) $ facts

randomFact :: (RandomGenerator m, OptionsConfig m) => m String
randomFact = do
  facts <- asks catFacts
  index <- (uncurry randRange  (A.bounds facts)) 
  return $ facts A.! index
