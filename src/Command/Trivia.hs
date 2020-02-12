{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Command.Trivia where

import OpentdbHelpers
import Conc
import Bot
import Bot.Random

import qualified Data.Text as T
import Data.List (sort)
import qualified Data.Map as M
import Control.Monad.State
import Control.Concurrent
import Control.Monad

prepareAnswers :: TdbResult -> ConcM App (StringType, StringType, StringType)
prepareAnswers triviaInfo =
  let
    correct     = unwrap . correct_answer $ triviaInfo
    wrong       = map unwrap . incorrect_answers $ triviaInfo
    answers     = correct : wrong
    optionCount = 1 + length wrong
  in do
    order <-
      pureM
      $   map (T.pack . pure)
      <$> (permutation . take optionCount $ ['A' ..])
    return
      ( head order
      , "(" <> head order <> ") " <> correct
      , T.unwords . sort $ zipWith (\a b -> "(" <> a <> ") " <> b) order answers
      )

triviaCommandM :: ConcM App ()
triviaCommandM = do
  trivias' <- opentdbQuery "multiple" 50
  case trivias' of
    Nothing      -> taskM (threadDelay 10000000)
    Just trivias -> mapM_ triviaM trivias
  triviaCommandM


triviaM :: TdbResult -> ConcM App ()
triviaM triviaInfo = do
  (correctOption, correct, formattedOptions) <- prepareAnswers triviaInfo
  awaitM_ [ChatCommand "!trivia"] 0
  messageM $ unwrap . question $ triviaInfo
  messageM formattedOptions
  let
    difficultyTime = case unwrap . difficulty $ triviaInfo of
      "easy"   -> 10
      "medium" -> 20
      "hard"   -> 30
      _        -> 10
  answers <- awaitMLoop
    [ChatCommand correctOption, ChatCommand (T.toLower correctOption)]
    (difficultyTime * 1000)
    (M.empty :: M.Map User StringType)
    (\case
      EventResult (ChatCommand (T.toUpper -> ans)) (ChatCommandResult message)
        -> do
          modify $ M.alter
            (\case
              Nothing -> Just ans
              Just e  -> Just e
            )
            (user message)
          return Nothing
      _ -> gets Just
    )
  messageM $ "The correct answer was: " <> correct

