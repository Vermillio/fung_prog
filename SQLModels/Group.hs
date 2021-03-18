{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SQLModels.Group (FromJSON (parseJSON), ToJSON (toJSON), Group(groupId, groupName), makeGroup, setGroupName, SQLModel (fetchOne, update, create, fetchAll))
  where

import SQLModels.SQLModel
import Utils

import Data.Maybe(fromMaybe)
import GHC.Generics
import Data.List
import Data.Int
import qualified Data.Text
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base
import Data.Aeson
import Control.Applicative

data Group = MakeGroup {groupId :: Integer, groupName :: String} deriving (Generic, Show)

makeGroup :: Integer -> String -> Group
makeGroup id name = MakeGroup id name

setGroupName :: Group -> String -> Group
setGroupName group name = makeGroup (groupId group) name

instance FromJSON Group where
  parseJSON (Object v) = MakeGroup  <$> v .:? "id" .!= 0
                                    <*> v .:? "name" .!= ""

instance ToJSON Group where
  toJSON (MakeGroup id name) = object ["id" .= id, "name" .= name]

mysqlToGroup :: [MySQLValue] -> Maybe Group
mysqlToGroup [MySQLInt64U id, MySQLText name] = Just $ makeGroup (fromIntegral id) (Data.Text.unpack name)
mysqlToGroup _ = Nothing

instance SQLModel Group where

  fetchOne conn groupId = do
    s <- prepareStmt conn "SELECT * FROM `groups_` where groups.id = ?;"
    (defs, is) <- queryStmt conn s $ toMySQLParams [groupId]
    rows <- Streams.toList is
    let selected = last rows
    putStrLn $ show selected
    let group = mysqlToGroup selected
    putStrLn $ show group
    return group :: IO (Maybe Group)

  update conn group = do
     execute conn q [toMySQL $ groupName group, toMySQL $ groupId group]
     where
       q = "UPDATE `groups_` SET name = ? where groups.id = ?;"

  create conn group = do
     execute conn q [toMySQL $ groupName group]
     where
       q = "insert into groups_ (name) values (?);"

  fetchAll conn = do
    (defs, is) <- query_ conn "SELECT * FROM `groups_`;"
    rows <- Streams.toList is
    let groups = map mysqlToGroup rows
    return $ map (fromMaybe $ makeGroup 0 "") groups
