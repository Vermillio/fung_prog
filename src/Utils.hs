{-# LANGUAGE FlexibleInstances #-}

module Utils where

import qualified System.IO.Streams as Streams
import qualified Data.Text
import qualified Data.ByteString.Char8    as BS
import Database.MySQL.Base ( MySQLValue(MySQLText, MySQLInt64U) )

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

fromMaybe :: Maybe a -> a
fromMaybe Nothing = error "Maybe.fromJust: Nothing"
fromMaybe (Just x) = x

unpack [MySQLInt64U id, MySQLText name] = (fromIntegral id, Data.Text.unpack name)
