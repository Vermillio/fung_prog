{-# LANGUAGE OverloadedStrings #-}

module SQLModels.Answer where

import SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

data Answer = MakeAnswer { answerId :: Integer, answerTeacherId :: Integer, answerQuestionId :: Integer, answerContent :: String }

makeAnswer :: Integer -> Integer -> Integer -> String -> Answer
makeAnswer id teacherId questionId content | length content > 500 = error "can't make"
                                              | otherwise = MakeAnswer id teacherId questionId content

setAnswerTeacherId :: Answer -> Integer -> Answer
setAnswerTeacherId answer teacherId = makeAnswer (answerId answer) teacherId (answerQuestionId answer) (answerContent answer)

setAnswerQuestionId :: Answer -> Integer -> Answer
setAnswerQuestionId answer questionId = makeAnswer (answerId answer) (answerQuestionId answer) questionId (answerContent answer)

setAnswerContent :: Answer -> String -> Answer
setAnswerContent answer content = makeAnswer (answerId answer) (answerQuestionId answer) (answerQuestionId answer) content

-- get conn answerId = do
--   s <- prepareStmt conn "SELECT * FROM `answers` where answers.id = ?"
--   (defs, is) <- queryStmt conn s $ toMySQLParams [answerId]
--   rows <- Streams.toList is
--   let selected = last (map unpack rows)
--   return $ makeAnswer (sel1 selected) (sel2 selected) (sel3 selected) (sel4 selected)
--
-- update conn answer = do
--    execute conn q [toMySQL $ answerTeacherId answer, toMySQL $ answerQuestionId answer, toMySQL $ answerContent answer, toMySQL $ answerId answer]
--    where
--      q = "UPDATE `answers` SET teacher_id = ?, question_id = ?, content = ? where answers.id = ?;"
--
-- create conn answer = do
--    execute conn q [toMySQL $ answerTeacherId answer, toMySQL $ answerQuestionId answer, toMySQL $ answerContent answer]
--    where
--      q = "insert into answers (teacher_id, question_id, content) values (?, ?, ?)"

-- fetchAll conn = do
--  s <- prepareStmt conn "SELECT * FROM 'answers'"
--  (defs, is) <- queryStmt conn s
--  rows <- Streams.toList is
--  let unpackedRows = map unpack rows
--  return zipWith4 makeAnswer (map sel1 unpackedRows) (map sel2 unpackedRows) (map sel3 unpackedRows) (map sel4 unpackedRows)
