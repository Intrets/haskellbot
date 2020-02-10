{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command.Nam where

import Bot

import Conc
import Control.Monad.State.Lazy

namCountingM :: ConcM App ()
namCountingM = do
  awaitM_ [ChatWord "NaM"] 0
  count <- awaitMLoop
    [ChatWord "NaM"]
    5
    (1 :: Int)
    (\case
      EventResult (ChatWord _) _ -> modify succ >> return Nothing
      _                          -> Just <$> get
    )

  return ()

namBountyM :: Int -> ConcM App ()
namBountyM count = do

  return ()
  --awaitMLoop [ChatWord "NaM"] 0
