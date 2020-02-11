{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command.Nam where

import Bot

import Bot.Database.Helpers
import Conc
import Control.Monad.State.Lazy
import MessageQueue
import Bot.Random

import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

import qualified Data.Array as A
import Text.Printf

import qualified Data.Set as S

import Control.Monad.Reader

parseNamLine :: StringType -> NamWord
parseNamLine line =
  let
    [lang, wrd] = T.splitOn ": " line
    s           = T.intersperse ' ' . T.replace "NAM" "___" . T.toUpper $ wrd
    prefix      = case lang of
      "english" -> "NaM :point_right: "
      "italian" -> ":spaghetti: :ok_hand: NaM :point_right: "
      _         -> "NaM :point_right: "
  in NamWord wrd (prefix <> s)

loadNams :: StringType -> IO (A.Array Int NamWord)
loadNams pth = do
  f <- map parseNamLine . T.lines <$> T.readFile (T.unpack pth)
  return $ A.listArray (0, pred . length $ f) f

namCountingM :: ConcM App ()
namCountingM = do
  awaitM_ [ChatWord "NaM"] 0
  count <- awaitMLoop
    [ChatWord "NaM"]
    5000
    (1 :: Int)
    (\case
      EventResult (ChatWord _) _ -> modify succ >> return Nothing
      _                          -> Just <$> get
    )
  forkM [namCountingM]
  (word_, msg) <- pureM $ do
    nams                           <- asks namWords
    NamWord word_ stylizedMessage_ <- pick nams
    return
      ( word_
      , stylizedMessage_ <> T.pack (printf " (%d POINTS :exclamation: )" count)
      )
  messageM msg
  namBountyM count word_

valid :: [StringType] -> StringType -> Bool
valid wrds wrd = let s = T.toLower . T.concat $ wrds in s == wrd

namBountyM :: Int -> StringType -> ConcM App ()
namBountyM count wrd = do
  r <- S.toList <$> awaitMLoop
    [ChatWord "NaM"]
    10000
    (S.empty :: S.Set User)
    (\case
      EventResult _ (ChatWordResult (Message wrds usr)) -> do
        when (valid wrds wrd) $ modify (S.insert usr)
        return Nothing
      _ -> Just <$> get
    )
  db <- pureM $ asks databaseOptions
  taskM $ multipleGivePoints db $ zip r (repeat count)




