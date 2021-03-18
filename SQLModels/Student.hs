{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SQLModels.Student (FromJSON (parseJSON), ToJSON (toJSON), Student(studentId, studentName, studentGroupId), makeStudent, setStudentName, setStudentGroupId, SQLModel (fetchOne, update, create, fetchAll))
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

data Student = MakeStudent {studentId :: Integer, studentName :: String, studentGroupId :: Integer} deriving (Generic, Show)

makeStudent :: Integer -> String -> Integer -> Student
makeStudent id name groupId = MakeStudent id name groupId

setStudentName :: Student -> String -> Student
setStudentName student name | name == "" = makeStudent (studentId student) (studentName student) (studentGroupId student)
                            | otherwise = makeStudent (studentId student) name (studentGroupId student)

setStudentGroupId :: Student -> Integer -> Student
setStudentGroupId student groupId | groupId > 0 = makeStudent (studentId student) (studentName student) groupId
                                  | otherwise = student

instance FromJSON Student where
  parseJSON (Object v) = MakeStudent  <$> v .:?  "id" .!= 0
                                      <*> v .:? "name" .!= ""
                                      <*> v .:? "group_id" .!= 0

instance ToJSON Student where
  toJSON (MakeStudent id name groupId) = object ["id" .= id, "name" .= name, "group_id" .= groupId]

-- makeStudentFromRow :: (Integer, String, Integer) -> Maybe Student
-- makeStudentFromRow row = Just $ makeStudent (sel1 row) (sel2 row) (sel3 row)
-- makeStudentFromRow _ = Nothing

mysqlToStudent :: [MySQLValue] -> Maybe Student
mysqlToStudent [MySQLInt64U id, MySQLText name, MySQLInt32 groupId] = Just $ makeStudent (fromIntegral id) (Data.Text.unpack name) (fromIntegral groupId)
mysqlToStudent _ = Nothing

instance SQLModel Student where

  fetchOne conn studentId = do
    s <- prepareStmt conn "SELECT * FROM `students` where students.id = ?;"
    (defs, is) <- queryStmt conn s $ toMySQLParams [studentId]
    rows <- Streams.toList is
    let selected = last rows
    putStrLn $ show selected
    let student = mysqlToStudent selected
    putStrLn $ show student
    return student :: IO (Maybe Student)

  update conn student = do
     execute conn q [toMySQL $ studentName student, toMySQL $ studentGroupId student, toMySQL $ studentId student]
     where
       q = "UPDATE `students` SET name = ?, group_id = ? where students.id = ?;"

  create conn student = do
     execute conn q [toMySQL $ studentName student, toMySQL $ studentGroupId student]
     where
       q = "INSERT INTO `students` (name, group_id) values (?, ?);"

  fetchAll conn = do
    (defs, is) <- query_ conn "SELECT * FROM `students`;"
    rows <- Streams.toList is
    let students = map mysqlToStudent rows
--    let student = makeStudent (sel1 row) (sel2 row) (sel3 row)
    return $ map (fromMaybe $ makeStudent 0 "" 0) students
