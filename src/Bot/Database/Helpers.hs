module Bot.Database.Helpers where

import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Dbsqlite3

import Bot
import Control.Monad.Reader

type UserID = Int

getConnection :: (OptionsConfig m, MonadIO m) => m Dbsqlite3.Connection
getConnection = do
  db <- asks (path . databaseOptions)
  liftIO $ Dbsqlite3.connectSqlite3 db

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

givePoints :: (OptionsConfig m, MonadIO m) => Int -> String -> Int -> m ()
givePoints userID name points = do
  connection <- getConnection
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
