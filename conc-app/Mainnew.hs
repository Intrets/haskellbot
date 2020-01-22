{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Bot
import Bot.Catfacts
import Bot.Irc
import Bot.Irc.Connection
import Bot.Irc.Send
import Bot.Options.Parse
import Bot.Random
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List
import GHC.IO.Encoding
import Options.Applicative
import System.IO
import System.Random

data Conc p
  = End
  | IOtask (IO (Conc p))
  | Pure (p (Conc p))

listenConc2 :: Conc App
listenConc2 =
  Pure $ do
    h <- asks (botSocket . bot)
    liftIO $ print "conc2"
    return $ IOtask $ (test h)
    -- >>= (return . pooThing) >> return listenConc2

{--
composing:
Conc' a -> (a ->Conc App' b) ==>
  do
    a <- io

--}
listenConc4 :: Conc App
listenConc4 =
  Pure $ do
    h <- asks (botSocket . bot)
    return $
      IOtask $ do
        line <- hGetLine $ h
        print $ "incoming message: " ++ line
        return $
          Pure $ do
            h <- asks (botSocket . bot)
            chan <- asks (ircChannel . programOptions)
            let s = init line
            if isPing s
              then return $ pong2 h s
              else return $
                   Pure $ do
                     privmsg2 "test"
                     return listenConc4
  where
    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

mainLoopSimple :: Conc App
mainLoopSimple =
  Pure $ do
    asks (botSocket . bot)
    return End

mainLoop :: Conc App
mainLoop =
  (asks (botSocket . bot) :: App Handle) $.
  hGetLine >-
  (\a -> do
     h <- asks (botSocket . bot)
     chan <- asks (ircChannel . programOptions)
     let s = init a
     privmsg2 ("test parrot " ++ a)) >+
  (const mainLoop)

--mainLoop = (\a -> hGetLine a) >. (end print)
listenConc31 :: Conc App -> Conc App
listenConc31 cont =
  Pure $ do
    h <- asks (botSocket . bot)
    liftIO $ print "conc3"
    return cont

listenConc21 :: App Handle
listenConc21 = do
  h <- asks (botSocket . bot)
  return h

listenConc22 :: Handle -> IO String
listenConc22 h = do
  line <- hGetLine h
  return line

class Composable n m
  -- (>.) :: (a -> m b) -> (b -> Conc n) -> (a -> Conc n)
                                                          where
  ($.) :: m b -> (b -> Conc n) -> Conc n
  end :: (a -> m b) -> (a -> Conc n)
  foreverC :: (a -> m a) -> (a -> Conc n)
  foreverC_ :: (() -> m ()) -> Conc n

instance Composable n IO where
  ($.) x y = IOtask $ do x >>= return . y
  end x a = IOtask $ do x a >> return End
  foreverC x = x >- foreverC x
  foreverC_ x = foreverC x ()

instance (Monad n) => Composable n n where
  ($.) x y = Pure $ do x >>= return . y
  end x a = Pure $ do x a >> return End
  foreverC x = x >+ foreverC x
  foreverC_ x = foreverC x ()

infixr 1 $.

(>-) :: (a -> IO b) -> (b -> Conc n) -> (a -> Conc n)
(>-) x y a = IOtask $ do x a >>= return . y

(>+) :: (Monad n) => (a -> n b) -> (b -> Conc n) -> (a -> Conc n)
(>+) x y a = Pure $ do x a >>= return . y

infixr 2 >-

infixr 2 >+

-- composeTest :: Conc App
-- composeTest = listenConc21 $. listenConc22 >.. (end print)
-- chain :: (a -> IO b) -> (b -> App b) -> (a -> App b)
-- chain a b = undefined
listenConc23 :: String -> App ()
listenConc23 line = do
  h <- asks (botSocket . bot)
  chan <- asks (ircChannel . programOptions)
  if "!test" `isPrefixOf` (clean line)
    then do
      privmsg2 "testing"
      return ()
    else return ()
  where
    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

privmsg2 :: (MonadIO m, MonadReader Options m) => [Char] -> m ()
privmsg2 msg = do
  chan <- asks (ircChannel . programOptions)
  write "PRIVMSG" (chan ++ " :" ++ msg)

test :: Handle -> IO (Conc App)
test h = do
  print "get"
  line <- hGetLine $ h
  print $ "incoming message: " ++ line
  return $ pooThing line

write2 :: (MonadIO m) => Handle -> String -> String -> m ()
write2 h cmd args = do
  let msg = cmd ++ " " ++ args ++ "\r\n"
  liftIO $ hPutStr h msg -- Send message on the wire
  liftIO $ putStr ("> " ++ msg) -- Show sent message on the command line

pooThing :: String -> Conc App
pooThing line =
  Pure $ do
    h <- asks (botSocket . bot)
    chan <- asks (ircChannel . programOptions)
    let s = init line
    if isPing s
      then return $ pong2 h s
      else return $ End
  where
    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

pong2 :: Handle -> String -> Conc App
pong2 h s =
  IOtask $ do
    write2 h "PONG" (':' : drop 6 s)
    return listenConc4

parrot :: Handle -> String -> String -> Conc App
parrot h chan s =
  IOtask $ do
    write2 h "PRIVMSG" (chan ++ " :" ++ take 15 s)
    return listenConc2

listenConc :: App ()
listenConc = do
  h <- asks (botSocket . bot)
  line <- liftIO $ hGetLine h
  liftIO (putStrLn line)
  let s = init line
  if isPing s
    then pong s
    else eval (clean s)
  where
    forever :: App () -> App ()
    forever a = do
      a
      forever a
    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1
    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x
    pong :: String -> App ()
    pong x = write "PONG" (':' : drop 6 x)

main :: IO ()
main = do
  print "start"
  setLocaleEncoding utf8
  hSetBuffering stdout NoBuffering
  options <- execParser clOptionsParser
  config <- parseConfigFile $ cfgFile options
  b <- connect (ircServer config) (ircPort config)
  let db = Database (dbFile config)
  s <- getStdGen
  catFacts <- loadFacts $ factsFile config
  let opt = Options b config db catFacts
  -- TODO turn into data type to chain computations together, split by something
  -- (a -> App b) ... (b -> App c) ... ...
  -- execute IO, wait for result, apply and next
  -- data Test = (IO a, a -> App Test)
  -- data Test = IO (App Test)
  -- queue <- atomically $ newTVar [slowIO 1 "1", slowIO 10 "10", slowIO 5 "5"]
  -- let test x =
  --       Task $ do
  --         _ <- slowIO x (show x)
  --         return . return $ End
  -- let test3 x =
  --       Task $ do
  --         _ <- slowIO 1 (show x)
  --         return . return $ test3 (succ x)
  queue <- atomically $ newTVar [leakTest2]
  _ <-
    (flip runReaderT) opt . (flip runStateT) 1 $ runApp (botJoin >> run queue)
  return ()
  -- queue <- atomically $ newTVar []
  -- print "start"
  -- replicateM_ 10 $ forkIO $ slowIO 2
  -- threadDelay 4000000
  -- print "end"

leakTest4 :: Conc App
leakTest4 = foreverC_ (const $ threadDelay 1)

leakTest :: Conc App
leakTest =
  (threadDelay 1) $.
  (const $
     -- r <- stateTest
     -- r <- randRange (1 :: Int) 10
    do return ()) >+
  (const leakTest)

leakTest2 :: Conc App
leakTest2 =
  IOtask $ do
    threadDelay 1
    return $
      Pure $ do
        liftIO $ print "1"
        return $ leakTest2

leakTest3 :: Conc App
leakTest3 =
  IOtask $ do
    threadDelay 1
    return leakTest3

run :: (TVar [Conc App]) -> App ()
run queue = do
  q <- liftIO $ atomically $ readTVar queue <* writeTVar queue []
  -- liftIO $ print $ length q
  case q of
    [] -> liftIO $ threadDelay 1
    _ -> do
      forM_ q $ \case
        End -> return ()
        IOtask io -> do
          _ <-
            liftIO . forkIO $ do
              a <- io
              atomically $ modifyTVar queue (a :)
          return ()
        Pure p -> do
          a <- p
          liftIO $ atomically $ modifyTVar queue (a :)
  run queue

slowIO :: Int -> String -> IO Int
slowIO seconds msg = do
  threadDelay $ seconds * 1000000
  print msg
  return 1
