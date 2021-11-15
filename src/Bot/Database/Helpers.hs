module Bot.Database.Helpers where

import qualified Database.HDBC                 as Db
import qualified Database.HDBC.Sqlite3         as Dbsqlite3

import qualified Data.Text                     as T
                                                ( unpack )

import           Bot
import           Control.Monad.Reader

type UserID = Int

getConnection :: (OptionsConfig m, MonadIO m) => m Dbsqlite3.Connection
getConnection = do
  db <- asks (path . databaseOptions)
  liftIO $ Dbsqlite3.connectSqlite3 (T.unpack db)

getConnectionIO :: Database -> IO Dbsqlite3.Connection
getConnectionIO = Dbsqlite3.connectSqlite3 . T.unpack . path

initializeDB :: (OptionsConfig m, MonadIO m) => m ()
initializeDB = do
  connection <- getConnection
  _          <- liftIO $ Db.run
    connection
    "CREATE TABLE IF NOT EXISTS users (user_id int PRIMARY KEY, display_name varchar(255), nam_points integer, trivia_points integer)"
    []
  liftIO $ Db.commit connection
  liftIO $ Db.disconnect connection

getTopNammers
  :: (OptionsConfig m, MonadIO m) => m [(User, Int)]
getTopNammers = do
  connection <- getConnection
  result     <- liftIO $ Db.quickQuery
    connection
    "SELECT user_id, display_name, nam_points FROM users ORDER BY nam_points DESC LIMIT 10"
    []
  let parsed = map (\[user_id, name, points] -> (User (Db.fromSql user_id) (Db.fromSql name), Db.fromSql points)) result
  return parsed 

multipleGiveNamPoints :: Database -> [(User, Int)] -> IO ()
multipleGiveNamPoints database t = do
  connection <- getConnectionIO database
  forM_
    t
    (\(User user_id display_name, namPoints) -> do
      _ <- liftIO $ Db.run
        connection
        "INSERT OR IGNORE INTO users (user_id, display_name, nam_points, trivia_points) VALUES (?, ?, ?, ?)"
        [ Db.toSql user_id
        , Db.toSql display_name
        , Db.toSql (0 :: Int)
        , Db.toSql (0 :: Int)
        ]
      _ <- liftIO $ Db.run
        connection
        "UPDATE users SET nam_points = nam_points + ? where user_id = ?"
        [Db.toSql namPoints, Db.toSql user_id]
      return ()
    )
  Db.commit connection
  Db.disconnect connection

multipleGiveTriviaPoints :: Database -> [(User, Int)] -> IO ()
multipleGiveTriviaPoints database t = do
  connection <- getConnectionIO database
  forM_
    t
    (\(User user_id display_name, triviaPoints) -> do
      _ <- liftIO $ Db.run
        connection
        "INSERT OR IGNORE INTO users (user_id, display_name, nam_points, trivia_points) VALUES (?, ?, ?, ?)"
        [ Db.toSql user_id
        , Db.toSql display_name
        , Db.toSql (0 :: Int)
        , Db.toSql (0 :: Int)
        ]
      _ <- liftIO $ Db.run
        connection
        "UPDATE users SET trivia_points = trivia_points + ? where user_id = ?"
        [Db.toSql triviaPoints, Db.toSql user_id]
      return ()
    )
  Db.commit connection
  Db.disconnect connection

getPoints
  :: (OptionsConfig m, MonadIO m) => Either Int String -> m (Maybe (Int, Int))
getPoints (Left userID) = do
  connection <- getConnection
  result     <- liftIO $ Db.quickQuery'
    connection
    "SELECT nam_points, trivia_points FROM users WHERE user_id = ?"
    [Db.toSql userID]
  liftIO $ Db.disconnect connection
  case result of
    [] -> return Nothing
    [[namPoints, triviaPoints]] ->
      return $ Just (Db.fromSql namPoints, Db.fromSql triviaPoints)
    _ -> return Nothing
getPoints (Right name) = do
  connection <- getConnection
  result     <- liftIO $ Db.quickQuery'
    connection
    "SELECT nam_points, trivia_points FROM users WHERE display_name = ?"
    [Db.toSql name]
  liftIO $ Db.disconnect connection
  case result of
    [] -> return Nothing
    [[namPoints, triviaPoints]] ->
      return $ Just (Db.fromSql namPoints, Db.fromSql triviaPoints)
    _ -> return Nothing
