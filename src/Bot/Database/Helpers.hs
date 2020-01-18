module Bot.Database.Helpers where

import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Dbsqlite3

type UserID = Int

class DatabaseImp a where
  getConnection :: a -> IO Dbsqlite3.Connection
  initializeDB :: a -> IO ()
  givePoints :: a -> UserID -> String -> Int -> IO ()
  getPoints :: a -> Either UserID String -> IO (Maybe Int)

data Database = Database
  { path :: String
  } deriving (Show)

instance DatabaseImp Database where
  getConnection db = Dbsqlite3.connectSqlite3 (path db)
  initializeDB db = do
    connection <- getConnection db
    Db.run
      connection
      "CREATE TABLE IF NOT EXISTS users (user_id int PRIMARY KEY, display_name varchar(255), points integer)"
      []
    Db.commit connection
    Db.disconnect connection
  givePoints db userID name points = do
    connection <- getConnection db
    Db.run
      connection
      "INSERT OR IGNORE INTO users (user_id, display_name, points) VALUES (?, ?, ?)"
      [Db.toSql userID, Db.toSql name, Db.toSql (0 :: Int)]
    Db.run
      connection
      "UPDATE users SET points = points + ? where user_id = ?"
      [Db.toSql points, Db.toSql userID]
    Db.commit connection
    Db.disconnect connection
  getPoints db (Left userID) = do
    connection <- getConnection db
    result <-
      Db.quickQuery'
        connection
        "SELECT points FROM users WHERE user_id = ?"
        [Db.toSql userID]
    Db.disconnect connection
    case result of
      [] -> return Nothing
      [[points]] -> return . Db.fromSql $ points
  getPoints db (Right name) = do
    connection <- getConnection db
    result <-
      Db.quickQuery'
        connection
        "SELECT points FROM users WHERE display_name = ?"
        [Db.toSql name]
    Db.disconnect connection
    case result of
      [] -> return Nothing
      [[points]] -> return . Db.fromSql $ points
