{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command.Nam where

import Bot

import Conc
import Control.Monad.State.Lazy
import MessageQueue

import Data.Text (pack)

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
  pureM $ queueMessage . pack . show $ count
  return ()

namBountyM :: Int -> ConcM App ()
namBountyM count = do

  return ()
  --awaitMLoop [ChatWord "NaM"] 0
