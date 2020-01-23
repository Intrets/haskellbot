module Bot.Database.Helpers where

import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Dbsqlite3

import qualified Data.Text as T (unpack)

import Bot
import Control.Monad.Reader

import Control.Concurrent

type UserID = Int

getConnection :: (OptionsConfig m, MonadIO m) => m Dbsqlite3.Connection
getConnection = do
  db <- asks (path . databaseOptions)
  liftIO $ Dbsqlite3.connectSqlite3 (T.unpack db)

getConnectionIO :: Database -> IO (Dbsqlite3.Connection)
getConnectionIO = Dbsqlite3.connectSqlite3 . T.unpack . path

initializeDB :: (OptionsConfig m, MonadIO m) => m ()
initializeDB = do
  connection <- getConnection
  _ <-
    liftIO $
    Db.run
      connection
      "CREATE TABLE IF NOT EXISTS users (user_id int PRIMARY KEY, display_name varchar(255), points integer)"
      []
  liftIO $ Db.commit connection
  liftIO $ Db.disconnect connection

givePoints :: Int -> String -> Int -> Database -> IO ()
givePoints userID name points database = do
  liftIO $ threadDelay 10000000
  connection <- getConnectionIO database
  _ <-
    liftIO $
    Db.run
      connection
      "INSERT OR IGNORE INTO users (user_id, display_name, points) VALUES (?, ?, ?)"
      [Db.toSql userID, Db.toSql name, Db.toSql (0 :: Int)]
  _ <-
    liftIO $
    Db.run
      connection
      "UPDATE users SET points = points + ? where user_id = ?"
      [Db.toSql points, Db.toSql userID]
  liftIO $ Db.commit connection
  liftIO $ Db.disconnect connection
  
getPointsIO :: StringType -> Database -> IO (Maybe Int)
getPointsIO name db = do
  connection <- getConnectionIO db
  result <-
    Db.quickQuery'
      connection
      "SELECT points FROM users WHERE display_name = ?"
      [Db.toSql name]
  Db.disconnect connection
  case result of
    [[points]] -> return . Db.fromSql $ points
    _ -> return Nothing

getPoints :: (OptionsConfig m, MonadIO m) => Either Int String -> m (Maybe Int)
getPoints (Left userID) = do
  connection <- getConnection
  result <-
    liftIO $
    Db.quickQuery'
      connection
      "SELECT points FROM users WHERE user_id = ?"
      [Db.toSql userID]
  liftIO $ Db.disconnect connection
  case result of
    [] -> return Nothing
    [[points]] -> return . Db.fromSql $ points
getPoints (Right name) = do
  connection <- getConnection
  result <-
    liftIO $
    Db.quickQuery'
      connection
      "SELECT points FROM users WHERE display_name = ?"
      [Db.toSql name]
  liftIO $ Db.disconnect connection
  case result of
    [] -> return Nothing
    [[points]] -> return . Db.fromSql $ points
