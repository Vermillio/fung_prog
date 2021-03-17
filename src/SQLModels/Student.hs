{-# LANGUAGE OverloadedStrings #-}

module SQLModels.Student where

import SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base
import Data.Aeson
import Control.Applicative

data Student = MakeStudent {studentId :: Integer, studentName :: String} deriving (Show)
--
makeStudent :: Integer -> String -> Student
makeStudent id name | length name > 20 = error "can't make"
                    | otherwise = MakeStudent id name

setStudentName :: Student -> String -> Student
setStudentName student = makeStudent (studentId student)

instance FromJSON Student where
  parseJSON (Object v) = MakeStudent <$>  v .:? "id" .!= 0 <*> v .:  "name"

instance ToJSON Student where
  toJSON (MakeStudent id name) = object ["id" .= id, "name" .= name]

makeStudentFromRow :: (Integer, String) -> Maybe Student
makeStudentFromRow row = Just $ makeStudent (sel1 row) (sel2 row)
makeStudentFromRow _ = Nothing

instance SQLModel Student where

  get conn studentId = do
    s <- prepareStmt conn "SELECT * FROM `students` where students.id = ?"
    (defs, is) <- queryStmt conn s $ toMySQLParams [studentId]
    rows <- Streams.toList is
    let selected = last (map unpack rows)
    return $ makeStudentFromRow selected

  update conn student = do
     execute conn q [toMySQL $ studentName student, toMySQL $ studentId student]
     where
       q = "UPDATE `students` SET name = ? where students.id = ?;"

  create conn student = do
     execute conn q [toMySQL $ studentName student]
     where
       q = "insert into students (name) values (?)"

  fetchAll conn = do
    s <- prepareStmt conn "SELECT * FROM 'students' where students.id = ?"
    let magic = 5 :: Integer
    (defs, is) <- queryStmt conn s $ toMySQLParams [magic]
    rows <- Streams.toList is
    let row = last( map unpack rows ) :: (Integer, String)
    let student = makeStudent (sel1 row) (sel2 row)
    return [ student ]
