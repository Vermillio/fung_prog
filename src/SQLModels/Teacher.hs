{-# LANGUAGE OverloadedStrings #-}

module SQLModels.Teacher where

import SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

data Teacher = MakeTeacher {teacherId :: Integer, teacherName :: String}

makeTeacher :: Integer -> String -> Teacher
makeTeacher id name | length name > 20 = error "can't make"
                    | otherwise = MakeTeacher id name

setTeacherName :: Teacher -> String -> Teacher
setTeacherName teacher = makeTeacher (teacherId teacher)

makeTeacherFromRow :: (Integer, String) -> Maybe Teacher
makeTeacherFromRow row = Just $ makeTeacher (sel1 row) (sel2 row)
makeTeacherFromRow _ = Nothing

instance SQLModel Teacher where

  get conn teacherId = do
    s <- prepareStmt conn "SELECT * FROM `teachers` where teachers.id = ?"
    (defs, is) <- queryStmt conn s $ toMySQLParams [teacherId]
    rows <- Streams.toList is
    let selected = last (map unpack rows)
    return $ makeTeacherFromRow selected

  update conn teacher = do
     execute conn q [toMySQL $ teacherName teacher, toMySQL $ teacherId teacher]
     where
       q = "UPDATE `teachers` SET name = ? where teachers.id = ?;"

  create conn teacher = do
     execute conn q [toMySQL $ teacherName teacher]
     where
       q = "insert into teachers (name) values (?)"

  -- fetchAll conn = do
  --   s <- prepareStmt conn "SELECT * FROM 'teachers'"
  --   (defs, is) <- queryStmt conn s
  --   rows <- Streams.toList is
  --   let unpackedRows = map unpack rows
  --   return zipWith makeTeacher (map sel1 unpackedRows) (map sel2 unpackedRows)
