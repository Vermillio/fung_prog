{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import           Control.Monad                        (join)
-- import           Control.Applicative                  ((<$>))
-- import           Controllers.Home                     (home, login, post)
import           Data.Maybe                           (fromMaybe)
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
-- import           System.Environment                   (lookupEnv)
import           Text.Read                            (readMaybe)
import           Web.Scotty


import            SQLModels.SQLModel
import            SQLModels.Student as Student
import            SQLModels.Group as Group
import            SQLModels.Teacher as Teacher
import            Views
import            Utils

import            Control.Monad.IO.Class
import            Data.Monoid (mconcat)
import qualified  Data.Configurator as C
import qualified  Data.Configurator.Types as C
import            Database.MySQL.Base
import            Data.Text.Lazy
import qualified  System.IO.Streams as Streams

main = do
  conn <- connect
    defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "lab1"}
  --     middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
  --     middleware logStdoutDev
  --     home >> login >> post
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico

    get "/test" $ html "Server is connected!"

    get "/students" $ do
      students <- liftAndCatchIO $ Student.fetchAll conn
      let s = students :: [Student]
      liftIO $ putStrLn $ show s
      json $ s

    get "/students/:" $ do
      id <- param "id" `rescue` (\_ -> return "0")
      let intId = Utils.readInt id :: Integer
      st <- liftAndCatchIO $ Student.fetchOne conn intId
      let s = Data.Maybe.fromMaybe (Student.makeStudent 0 "" 0) st :: Student
      liftIO $ putStrLn $ show s
      json $ s
      -- liftIO $ putStrLn $ Student.studentName s

    get "/groups" $ do
      groups <- liftAndCatchIO $ Group.fetchAll conn
      let g = groups :: [Group]
      liftIO $ putStrLn $ show g
      json $ g

    get "/groups/:" $ do
      id <- param "id" `rescue` (\_ -> return "0")
      let intId = Utils.readInt id :: Integer
      st <- liftAndCatchIO $ Group.fetchOne conn intId
      let s = Data.Maybe.fromMaybe (Group.makeGroup 0 "") st :: Group
      liftIO $ putStrLn $ show s
      json $ s

    get "/teachers" $ do
      teachers <- liftAndCatchIO $ Teacher.fetchAll conn
      let g = teachers :: [Teacher]
      liftIO $ putStrLn $ show g
      json $ g

    get "/teachers/:" $ do
      id <- param "id" `rescue` (\_ -> return "0")
      let intId = Utils.readInt id :: Integer
      st <- liftAndCatchIO $ Teacher.fetchOne conn intId
      let s = Data.Maybe.fromMaybe (Teacher.makeTeacher 0 "" 0 0) st :: Teacher
      liftIO $ putStrLn $ show s
      json $ s

    post "/students/create/:" $ do
      (name_ :: String) <- param "name"
      (groupId_ :: Integer) <- param "group_id"
      res <- liftAndCatchIO (Student.create conn (Student.makeStudent 0 name_ groupId_) :: IO OK)
      liftIO $ putStrLn $ show res
      Web.Scotty.text $ pack(show res)

    post "/groups/create/:" $ do
      (name_ :: String) <- param "name"
      res <- liftAndCatchIO (Group.create conn (Group.makeGroup 0 name_) :: IO OK)
      Web.Scotty.text $ pack(show res)

    post "/teachers/create/:" $ do
      (name_ :: String) <- param "name"
      (workHoursStart :: Integer) <- param "working_hours_start"
      (workHoursEnd :: Integer) <- param "working_hours_end"
      let createdTeacher = Teacher.makeTeacher 0 name_ workHoursStart workHoursEnd
      res <- liftAndCatchIO (Teacher.create conn createdTeacher :: IO OK)
      liftIO $ putStrLn $ show res
      Web.Scotty.text $ pack(show res)

    post "/students/update/:" $ do
      id <- param "id" `rescue` (\_ -> return "0")
      let intId = Utils.readInt id :: Integer
      studentJson <- jsonData :: ActionM Student
      studentMaybe <- liftAndCatchIO $ Group.fetchOne conn intId
      let student = Data.Maybe.fromMaybe (Student.makeStudent 0 "" 0) studentMaybe :: Student
      let studentUpdated = setStudentGroupId (setStudentName student $ studentName studentJson) $ studentGroupId studentJson
      r <- liftAndCatchIO (Student.update conn studentUpdated :: IO OK)
      html $ pack (show r)

    post "/teachers/update/:" $ do
      id <- param "id" `rescue` (\_ -> return "0")
      let intId = Utils.readInt id :: Integer
      teacherJson <- jsonData :: ActionM Teacher
      teacherMaybe <- liftAndCatchIO $ Teacher.fetchOne conn intId
      let teacher = Data.Maybe.fromMaybe (Teacher.makeTeacher 0 "" 0 0) teacherMaybe :: Teacher
      let teacherUpdated = setTeacherWorkingHours (setTeacherName teacher $ teacherName teacherJson) (workingHoursStart teacherJson) (workingHoursEnd teacherJson)
      r <- liftAndCatchIO $ Student.update conn teacherUpdated
      html $ pack (show r)

    post "/groups/update/:" $ do
      id <- param "id" `rescue` (\_ -> return "0")
      let intId = Utils.readInt id :: Integer
      groupJson <- jsonData :: ActionM Group
      groupMaybe <- liftAndCatchIO $ Group.fetchOne conn intId
      let group = Data.Maybe.fromMaybe (Group.makeGroup 0 "") groupMaybe :: Group
      let groupUpdated = setGroupName group $ groupName groupJson
      r <- liftAndCatchIO $ Student.update conn groupUpdated
      html $ pack (show r)
