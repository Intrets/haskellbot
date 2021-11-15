{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command.Nam where

import Bot

import Bot.Database.Helpers
import Conc
import Control.Monad.State.Lazy
import Bot.Random

import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

import qualified Data.Array as A
import Text.Printf

import qualified Data.Set as S

import Control.Monad.Reader

point_right :: StringType
point_right = TE.decodeUtf8 "\195\176\194\159\194\145\194\137"

spaghetti :: StringType
spaghetti = TE.decodeUtf8 "\195\176\194\159\194\141\194\157"

ok_hand :: StringType
ok_hand = TE.decodeUtf8 "\195\176\194\159\194\145\194\140"

exclamation :: StringType
exclamation = TE.decodeUtf8 "\195\162\194\157\194\151"

pinched_fingers :: StringType 
pinched_fingers = TE.decodeUtf8 "\195\176\194\159\194\164\194\140"  

parseNamLine :: StringType -> NamWord
parseNamLine line =
  let
    [lang, wrd] = T.splitOn ": " line
    s           = T.replace "NAM" " _ _ _ " . T.toUpper $ wrd
    prefix      = case lang of
      "english" -> "NaM " <> point_right <> " "
      "italian" -> spaghetti <> " NaM " <> pinched_fingers
      _         -> ""
  in NamWord wrd (prefix <> s)

loadNams :: StringType -> IO (A.Array Int NamWord)
loadNams pth = do
  f <- map parseNamLine . T.lines <$> T.readFile (T.unpack pth)
  return $ A.listArray (0, pred . length $ f) f

namCountingM :: ConcM App ()
namCountingM = do
  awaitM_ [ChatWord "NaM"] 0
  pureM $ liftIO $ print "new nam"
  count <- awaitMLoop
    [ChatWord "NaM"]
    10000
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
      , stylizedMessage_
      <> T.pack (printf " (%d POINTS " count)
      <> exclamation
      <> " )"
      )
  messageM msg
  namBountyM count word_

valid :: [StringType] -> StringType -> Bool
valid wrds wrd =
  let
    s = T.toLower . T.concat $ wrds
    t = T.toLower . T.concat . T.words $ wrd
  in s == t

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
  taskM $ multipleGiveNamPoints db $ zip r (repeat count)




