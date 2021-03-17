{-# LANGUAGE OverloadedStrings #-}

module SQLModels.Question where

import SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

data Question = MakeQuestion { questionId :: Integer, questionStudentId :: Integer, questionThemeId :: Integer, questionContent :: String }

makeQuestion :: Integer -> Integer -> Integer -> String -> Question
makeQuestion id studentId themeId content | length content > 500 = error "can't make"
                                          | otherwise = MakeQuestion id studentId themeId content

setQuestionStudentId :: Question -> Integer -> Question
setQuestionStudentId question studentId = makeQuestion (questionId question) studentId (questionThemeId question) (questionContent question)

setQuestionThemeId :: Question -> Integer -> Question
setQuestionThemeId question themeId = makeQuestion (questionId question) (questionStudentId question) themeId (questionContent question)

setQuestionContent :: Question -> String -> Question
setQuestionContent question content = makeQuestion (questionId question) (questionStudentId question) (questionThemeId question) content

-- get conn questionId = do
--   s <- prepareStmt conn "SELECT * FROM `questions` where questions.id = ?"
--   (defs, is) <- queryStmt conn s $ toMySQLParams [questionId]
--   rows <- Streams.toList is
--   let selected = last (map unpack rows)
--   return $ makeQuestion (sel1 selected) (sel2 selected) (sel3 selected) (sel4 selected)
--
-- update conn question = do
--    execute conn q [toMySQL $ questionStudentId question, toMySQL $ questionThemeId question, toMySQL $ questionContent question, toMySQL $ questionId question]
--    where
--      q = "UPDATE `questions` SET student_id = ?, theme_id = ?, content = ? where questions.id = ?;"
--
-- create conn question = do
--    execute conn q [toMySQL $ questionStudentId question, toMySQL $ questionThemeId question, toMySQL $ questionContent question]
--    where
--      q = "insert into questions (student_id, theme_id, content) values (?, ?, ?)"

-- fetchAll conn = do
--   s <- prepareStmt conn "SELECT * FROM 'questions'"
--   (defs, is) <- queryStmt conn s
--   rows <- Streams.toList is
--   let unpackedRows = map unpack rows
--   return zipWith4 makeQuestion (map sel1 unpackedRows) (map sel2 unpackedRows) (map sel3 unpackedRows) (map sel4 unpackedRows)
