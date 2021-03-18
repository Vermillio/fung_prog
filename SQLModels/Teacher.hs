{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SQLModels.Teacher (FromJSON (parseJSON), ToJSON (toJSON), Teacher(teacherId, teacherName, workingHoursStart, workingHoursEnd), makeTeacher, setTeacherName, setTeacherWorkingHours, SQLModel (fetchOne, update, create, fetchAll))
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

data Teacher = MakeTeacher {teacherId :: Integer, teacherName :: String, workingHoursStart :: Integer, workingHoursEnd :: Integer} deriving (Generic, Show)

makeTeacher :: Integer -> String -> Integer -> Integer -> Teacher
makeTeacher id name whs whe = MakeTeacher id name whs whe

setTeacherName :: Teacher -> String -> Teacher
setTeacherName teacher name = makeTeacher (teacherId teacher) name (workingHoursStart teacher) (workingHoursEnd teacher)

setTeacherWorkingHours :: Teacher -> Integer -> Integer -> Teacher
setTeacherWorkingHours teacher whs whe | whs < 0 || whs > 24 || whe < 0 || whe > 24 = teacher
                                       | otherwise = makeTeacher (teacherId teacher) (teacherName teacher) whs whe
instance FromJSON Teacher where
  parseJSON (Object v) = MakeTeacher  <$> v .:? "id" .!= 0
                                      <*> v .:? "name" .!= ""
                                      <*> v .:? "working_hours_start" .!= 0
                                      <*> v .:? "working_hours_end" .!= 0

instance ToJSON Teacher where
  toJSON (MakeTeacher id name whs whe) = object ["id" .= id, "name" .= name, "working_hours_start" .= whs, "working_hours_end" .= whe]

mysqlToTeacher :: [MySQLValue] -> Maybe Teacher
mysqlToTeacher [MySQLInt64U id, MySQLText name, MySQLInt32 whs, MySQLInt32 whe] = Just $ makeTeacher (fromIntegral id) (Data.Text.unpack name) (fromIntegral whs) (fromIntegral whe)
mysqlToTeacher _ = Nothing

instance SQLModel Teacher where

  fetchOne conn teacherId = do
    s <- prepareStmt conn "SELECT * FROM `teachers_` where teachers.id = ?;"
    (defs, is) <- queryStmt conn s $ toMySQLParams [teacherId]
    rows <- Streams.toList is
    let selected = last rows
    putStrLn $ show selected
    let teacher = mysqlToTeacher selected
    putStrLn $ show teacher
    return teacher :: IO (Maybe Teacher)

  update conn teacher = do
     execute conn q [toMySQL $ teacherName teacher, toMySQL $ workingHoursStart teacher, toMySQL $ workingHoursEnd teacher, toMySQL $ teacherId teacher]
     where
       q = "UPDATE `teachers` SET name = ?, working_hours_start = ?, working_hours_end = ? where teachers.id = ?;"

  create conn teacher = do
     execute conn q [toMySQL $ teacherName teacher, toMySQL $ workingHoursStart teacher, toMySQL $ workingHoursEnd teacher]
     where
       q = "insert into teachers (name, working_hours_start, working_hours_end) values (?);"

  fetchAll conn = do
    (defs, is) <- query_ conn "SELECT * FROM `teachers`;"
    rows <- Streams.toList is
    let teachers = map mysqlToTeacher rows
    return $ map (fromMaybe $ makeTeacher 0 "" 0 0) teachers
