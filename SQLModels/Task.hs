{-# LANGUAGE OverloadedStrings #-}

module SQLModels.Task where

import SQLModels.SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

data Task = MakeTask { taskId :: Integer, taskName :: String, taskContent :: String, taskStudentId :: Integer, taskThemeId :: Integer, taskDone :: Bool, taskGrade :: Integer, taskLinkToWork :: String }

makeTask :: Integer -> String -> String -> Integer -> Integer -> Bool -> Integer -> String -> Task
makeTask id name content studentId themeId done grade linkToWork | length name > 120 = error "can't make"
                                                               | length linkToWork > 200 = error "can't make"
                                                               | otherwise = MakeTask id name content studentId themeId done grade linkToWork

setTaskName :: Task -> String -> Task
setTaskName task name = makeTask (taskId task) name (taskContent task) (taskStudentId task) (taskThemeId task) (taskDone task) (taskGrade task) (taskLinkToWork task)

setTaskContent :: Task -> String -> Task
setTaskContent task content = makeTask (taskId task) (taskName task) content (taskStudentId task) (taskThemeId task) (taskDone task) (taskGrade task) (taskLinkToWork task)

setTaskStudentId :: Task -> Integer -> Task
setTaskStudentId task studentId = makeTask (taskId task) (taskName task) (taskContent task) studentId (taskThemeId task) (taskDone task) (taskGrade task) (taskLinkToWork task)

setTaskThemeId :: Task -> Integer -> Task
setTaskThemeId task themeId = makeTask (taskId task) (taskName task) (taskContent task) (taskStudentId task) themeId (taskDone task) (taskGrade task) (taskLinkToWork task)

setTaskDone :: Task -> Bool -> Task
setTaskDone task done = makeTask (taskId task) (taskName task) (taskContent task) (taskStudentId task) (taskThemeId task) done (taskGrade task) (taskLinkToWork task)

setTaskGrade :: Task -> Integer -> Task
setTaskGrade task grade = makeTask (taskId task) (taskName task) (taskContent task) (taskStudentId task) (taskThemeId task) (taskDone task) grade (taskLinkToWork task)

setTaskLinkToWork :: Task -> String -> Task
setTaskLinkToWork task linkToWork = makeTask (taskId task) (taskName task) (taskContent task) (taskStudentId task) (taskThemeId task) (taskDone task) (taskGrade task) linkToWork

-- get conn taskId = do
--   s <- prepareStmt conn "SELECT * FROM `tasks` where tasks.id = ?"
--   (defs, is) <- queryStmt conn s $ toMySQLParams [taskId]
--   rows <- Streams.toList is
--   let selected = last (map unpack rows)
--   return $ makeTask (sel1 selected) (sel2 selected) (sel3 selected) (sel4 selected) (sel5 selected) (sel6 selected) (sel7 selected) (sel8 selected)

-- update conn task = do
--    execute conn q [toMySQL $ taskName task, toMySQL $ taskContent task, toMySQL $ taskStudentId task, toMySQL $ taskThemeId task, toMySQL $ fromEnum $ taskDone task, toMySQL $ taskGrade task, toMySQL $ taskLinkToWork task, toMySQL $ taskId task]
--    where
--      q = "UPDATE `tasks` SET name = ?, content = ?, student_id = ?, theme_id = ?, done = ?, grade = ?, link_to_work = ? where tasks.id = ?;"
--
-- create conn task = do
--    execute conn q [toMySQL $ taskName task, toMySQL $ taskContent task, toMySQL $ taskStudentId task, toMySQL $ taskThemeId task, toMySQL $ fromEnum $ taskDone task, toMySQL $ taskGrade task, toMySQL $ taskLinkToWork task]
--    where
--      q = "insert into tasks (name, content, student_id, theme_id, done, grade, link_to_work) values (?, ?, ?, ?, ?, ?, ?)"
--

 -- fetchAll conn = do
 --   s <- prepareStmt conn "SELECT * FROM 'tasks'"
 --   (defs, is) <- queryStmt conn s
 --   rows <- Streams.toList is
 --   let unpackedRows = map unpack rows
 --   return zipWith8 makeTask (map sel1 unpackedRows) (map sel2 unpackedRows) (map sel3 unpackedRows) (map sel4 unpackedRows) (map sel5 unpackedRows) (map sel6 unpackedRows) (map sel7 unpackedRows) (map sel8 unpackedRows)
