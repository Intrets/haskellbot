module Bot.Catfacts where

import Bot
import Bot.Random

import Control.Monad.Reader
import qualified Data.Vector as V
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

loadFacts :: String -> IO (V.Vector String)
loadFacts path = do
  file <- openFile path ReadMode
  facts <- filter (not . null) . lines <$> hGetContents file
  hClose file
  return . V.fromList $ facts

randomFact :: (RandomGenerator m, OptionsConfig m) => m String
randomFact = do
  facts <- asks catFacts
  -- index <- randRange 0 (V.length facts)
  let index = -1
  return $ facts V.! index
