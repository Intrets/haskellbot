module Main where

import Bot.Database.Helpers

import Control.Applicative
import Control.Exception -- base
import Control.Monad.IO.Class --
import Control.Monad.Trans.Reader -- transformers
import Data.List --
import Data.List.Split
import qualified Data.Map as Map
import Data.Monoid
import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Dbsqlite3
import qualified Network.Socket as N -- network
import Options.Applicative
import System.Exit --
import System.IO --
import Text.Printf

argparser :: Parser String
argparser =
  strOption
    (long "config" <> short 'c' <> metavar "CONFIGFILE" <> value "default.cfg")

data CLOptions = CLOptions
  { cfgFile :: String
  }
  
clOptions = CLOptions <$> argparser

clOptionsParser =
  info
    (clOptions <**> helper)
    (fullDesc <> progDesc "chat bot test" <> header "this is a header")

data ProgramOptions = ProgramOptions
  { ircServer :: String
  , ircPort :: N.PortNumber
  , ircChannel :: String
  , ircNick :: String
  , ircOauth :: String
  , dbFile :: String
  } deriving (Show)

parseConfigFile :: String -> IO ProgramOptions
parseConfigFile path = do
  file <- openFile path ReadMode
  contents <- lines <$> hGetContents file
  let getOption =
        (Map.!) . Map.fromList . map ((\[a, b] -> (a, b)) . splitOn "=") $
        contents
  let res =
        ProgramOptions
          (getOption "ircServer")
          (read $ getOption "ircPort")
          (getOption "ircChannel")
          (getOption "ircNick")
          (getOption "ircOauth")
          (getOption "dbFile")
  return res

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = do
  options <- execParser clOptionsParser
  config <- parseConfigFile $ cfgFile options
  bot <- connect (ircServer config) (ircPort config)
  let db = Database (dbFile config)
  initializeDB db
  bracket (pure $ App bot config db) disconnect loop
  where
    disconnect = hClose . botSocket . bot
    loop st = runReaderT run st

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
data Bot = Bot
  { botSocket :: Handle
  }

data App = App
  { bot :: Bot
  , programOptions :: ProgramOptions
  , databaseOptions :: Database
  }



--  givePoints x y z = _givePoints_body
type Net = ReaderT App IO

-- Connect to the server and return the initial bot state
connect :: String -> N.PortNumber -> IO Bot
connect server port =
  notify $ do
    h <- connectTo server port
    return (Bot h)
  where
    notify a =
      bracket_
        (putStrLn ("Connecting to " ++ server ++ " ...") >> hFlush stdout)
        (putStrLn "done.")
        a

-- Connect to the server and return a Handle (helper for connect)
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
  addr:_ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  sock <-
    N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
  N.connect sock (N.addrAddress addr)
  N.socketToHandle sock ReadWriteMode

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  oauth <- asks (ircOauth . programOptions)
  write "PASS" ("oauth:" ++ oauth)
  nick <- asks (ircNick . programOptions)
  write "NICK" nick
  write "USER" (nick ++ " 0 * :tutorial bot")
  chan <- asks (ircChannel . programOptions)
  write "JOIN" chan
  listen

-- Send a message to the server we're currently connected to
write :: String -> String -> Net ()
write cmd args = do
  h <- asks (botSocket . bot)
  let msg = cmd ++ " " ++ args ++ "\r\n"
  liftIO $ hPutStr h msg -- Send message on the wire
  liftIO $ putStr ("> " ++ msg) -- Show sent message on the command line

-- Process each line from the server
listen :: Net ()
listen =
  forever $ do
    h <- asks (botSocket . bot)
    line <- liftIO $ hGetLine h
    liftIO (putStrLn line)
    let s = init line
    if isPing s
      then pong s
      else eval (clean s)
  where
    forever :: Net () -> Net ()
    forever a = do
      a
      forever a
    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1
    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x
    pong :: String -> Net ()
    pong x = write "PONG" (':' : drop 6 x)

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(a:rest) !? 0 = Just a
(a:rest) !? n = rest !? (n - 1)

-- Dispatch a command
eval :: String -> Net ()
eval "!test" = privmsg "hello"
eval "!quit" = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval x
  | "!id " `isPrefixOf` x = privmsg (drop 4 x)
  | "!points" `isPrefixOf` x = do
    case (words x) !? 1 of
      Nothing -> privmsg "user not found"
      Just name -> do
        db <- asks databaseOptions
        points_ <- liftIO $ getPoints db (Right name)
        case points_ of
          Just points -> privmsg $ printf "user %s has %d points" name points
          Nothing -> privmsg $ printf "user %s not found" name
  | "!give" `isPrefixOf` x = do
    case (words x) !? 1 of
      Nothing -> privmsg "user not found"
      Just name -> do
        db <- asks databaseOptions
        liftIO $ givePoints db 1 name 1
eval _ = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg msg = do
  chan <- asks (ircChannel . programOptions)
  write "PRIVMSG" (chan ++ " :" ++ msg)
