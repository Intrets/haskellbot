module Bot.Catfacts where

import Bot
import Bot.Random

import Control.Monad.Reader
import qualified Data.Array as A
import qualified Data.Text as T (lines, null, pack, strip, unpack)
import qualified Data.Text.IO as T (readFile)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

loadFacts :: StringType -> IO (A.Array Int StringType)
loadFacts path = do
  facts <- filter (not . T.null) . map T.strip . T.lines <$> T.readFile
    (T.unpack path)
  return . A.listArray (0, pred . length $ facts) $ facts

randomFact :: (MonadIO m, RandomGenerator m, OptionsConfig m) => m StringType
randomFact = do
  facts <- asks catFacts
  index <- uncurry randRange (A.bounds facts)
  return $ facts A.! index
