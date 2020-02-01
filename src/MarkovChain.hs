{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkovChain where

import Bot
import Bot.Random
import qualified Data.Array as A
import Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.List.Split
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

data (Hashable a, A.Ix b) =>
     Indexed a b = Indexed
  { toIndex :: a -> Maybe b
  , fromIndex :: b -> a
  }

-- data MarkovChain = MarkovChain
--   { rawWordsIndex :: Indexed StringType Int
--   , cleanedWordsIndex :: Indexed StringType Int
--   , markovChain :: M.HashMap (Int, Int) Int
--   , predict :: (StringType, StringType) -> IO (Maybe StringType)
--   }
cleanWord :: StringType -> StringType
cleanWord = T.filter isAlphaNum

makeRawWordsIndex :: [StringType] -> Indexed StringType Int
makeRawWordsIndex words = Indexed (`M.lookup` toIndexMap) (fromIndexMap A.!)
 where
  toIndexMap   = M.fromList $ zip words [0 ..]
  fromIndexMap = A.listArray (0, pred . M.size $ toIndexMap) words

makeCleanedWordsIndex :: [StringType] -> Indexed StringType Int
makeCleanedWordsIndex words = Indexed
  (flip M.lookup toIndexMap)
  (fromIndexMap A.!)
 where
  cleanedwords = S.toList . S.fromList . map cleanWord $ words
  toIndexMap   = M.fromList $ zip cleanedwords [0 ..]
  fromIndexMap = A.listArray (0, pred . M.size $ toIndexMap) cleanedwords

liftMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
liftMaybe (Just a, Just b) = Just (a, b)
liftMaybe _                = Nothing

createMarkovChain
  :: StringType -> IO ((StringType, StringType) -> App (Maybe StringType))
createMarkovChain path = do
  contents <- T.lines <$> (T.readFile . T.unpack $ path)
  let rawWordsIndex     = makeRawWordsIndex contents
  let cleanedWordsIndex = makeCleanedWordsIndex contents
  let
    pairs = concatMap
      (\line ->
        (map (\[a, b] -> ((a, a), b)) . chunksOf 2 . T.words $ line)
          ++ (map (\[a, b, c] -> ((a, b), c)) . chunksOf 3 . T.words $ line)
      )
      contents
  let
    pairs2 = map
      (\((a, b), c) ->
        ( ( (fromJust . toIndex cleanedWordsIndex) a
          , (fromJust . toIndex cleanedWordsIndex) b
          )
        , (fromJust . toIndex rawWordsIndex) c
        )
      )
      pairs
  let
    m =
      M.map (\l -> A.listArray (0, pred . length $ l) l)
        $ M.fromListWith (++)
        $ [ (k, [v]) | (k, v) <- pairs2 ]
  let
    predict :: (StringType, StringType) -> App (Maybe StringType)
    predict (word1, word2) =
      let
        key = liftMaybe
          (toIndex cleanedWordsIndex word1, toIndex cleanedWordsIndex word2)
        r = (key >>= (flip M.lookup m))
      in case r of
        Nothing    -> return Nothing
        Just array -> Just . (fromIndex rawWordsIndex) <$> pick array
  return predict

generateFact :: [StringType] -> App [StringType]
generateFact seed = do
  let
    start = case seed of
      [] -> 1
  return []
