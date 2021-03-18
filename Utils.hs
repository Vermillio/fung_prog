{-# LANGUAGE FlexibleInstances #-}

module Utils where

import           Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Char8    as BS
import           Data.Text (Text, pack, unpack)

class MySQLConverter a where
    toMySQL :: a -> MySQLValue

instance MySQLConverter String where
    toMySQL s = MySQLText $ Data.Text.pack s

instance MySQLConverter Integer  where
    toMySQL i = MySQLInt64U $ fromIntegral i

instance MySQLConverter Int  where
    toMySQL i = MySQLInt64U $ fromIntegral i

toMySQLParams :: (MySQLConverter p) => [p] -> [MySQLValue]
toMySQLParams list =  [toMySQL (last list)]

readInt :: Text -> Integer
readInt t = read $ Data.Text.unpack t
