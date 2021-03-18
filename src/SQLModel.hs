module SQLModel where

import qualified System.IO.Streams as Streams
import Database.MySQL.Base

class SQLModel a where
  get :: MySQLConn -> Integer -> IO (Maybe a)
  fetchAll :: MySQLConn -> IO [a]
  update :: MySQLConn -> a -> IO OK
  create :: MySQLConn -> a -> IO OK
