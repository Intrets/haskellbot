{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Command.CursedCommand
  ( activateTrivia
  )
where

import Bot
import Conc
import OpentdbHelpers

import qualified Data.Set as S

import Control.Monad.State

activateTrivia :: ConcM App ()
activateTrivia = do
  _ <- awaitM_ [ChatCommand "!df"] 0
  dubiousFactTrivia

dubiousFactTrivia :: ConcM App ()
dubiousFactTrivia = do
  fetch <- opentdbQuery "boolean" 1
  case fetch of
    Nothing           -> activateTrivia
    Just []           -> activateTrivia
    Just (result : _) -> do
      let answer = unwrap $ correct_answer result
      messageM $ unwrap . question $ result
      (correct, _) <- awaitMLoop
        [ChatCommand "True", ChatCommand "False"]
        10000
        S.empty
        (\case
          EventResult (ChatCommand x) (ChatCommandResult (Message _ usr)) -> do
            alreadyAnswered <- gets $ S.member usr
            if
              | alreadyAnswered -> return Nothing
              | answer == x     -> gets $ Just . (Just usr, )
              | otherwise       -> modify (S.insert usr) >> return Nothing
          EventResult Timeout TimeoutResult -> gets $ Just . (Nothing, )
          _ -> return Nothing
        )
      case correct of
        Nothing ->
          messageM
            $  "the correct answer was "
            <> (unwrap . correct_answer $ result)
        Just m -> messageM $ displayName m <> " answered correctly"
      activateTrivia

