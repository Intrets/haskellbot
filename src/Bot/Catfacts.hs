module Bot.Catfacts where

import Bot
import Bot.Random

import Control.Monad.Reader
import qualified Data.Array as A
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

loadFacts :: StringType -> IO (A.Array Int StringType)
loadFacts pth = do
  facts <- filter (not . T.null) . map T.strip . T.lines <$> T.readFile
    (T.unpack pth)
  return . A.listArray (0, pred . length $ facts) $ facts

randomFact :: (MonadIO m, RandomGenerator m, OptionsConfig m) => m StringType
randomFact = do
  facts <- asks catFacts
  index <- uncurry randRange (A.bounds facts)
  return $ facts A.! index
