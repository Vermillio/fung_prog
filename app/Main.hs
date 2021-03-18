
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SQLModel
import qualified SQLModels.Student
import Views
import Utils

import Control.Monad.IO.Class
import Data.Monoid (mconcat)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.MySQL.Base
import Data.Text
import qualified System.IO.Streams as Streams

-- data DbConfig = DbConfig {
--      dbName :: String,
--      dbUser :: String,
--      dbPassword :: String
--      }
--      deriving (Show)
--
-- makeDbConfig :: C.Config -> IO (Maybe DbConfig)
-- makeDbConfig conf = do
--   name <- C.lookup conf "database.name" :: IO (Maybe String)
--   user <- C.lookup conf "database.user" :: IO (Maybe String)
--   password <- C.lookup conf "database.password" :: IO (Maybe String)
--   return $ DbConfig <$> name
--                     <*> user
--                     <*> password
--
-- main = do
--   loadedConf <- C.load [C.Required "app.conf"]
--   dbConf <- makeDbConfig loadedConf
--   conn <- connect
--       defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "lab1"}
--   scotty 3000 $ do
--     -- get "/students" $ do
--     --   students <- SQLModel.fetchAll conn
--     --   viewStudents students
--
--     get "students/:id" $ do
--       id <- param "id" :: ActionM String
--       let intId = read id :: Integer
--       student <- liftIO $ SQLModel.get conn intId
--       viewStudent student

main :: IO ()
main = do
  conn <- connect
      defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "lab1"}
  let res = SQLModel.get conn 1
  student <- res
  let studentChanged = SQLModels.Student.setStudentName (Utils.fromMaybe student) "Mark"
  SQLModel.create conn studentChanged
  print $ SQLModels.Student.studentName studentChanged

--       pp <- res
--       print $ getName pp
--
--       let changed = setName pp "giovanni d"
--       print $ getName changed
--
--       let res2 = Entity.updateEntity conn changed :: IO OK
--       print =<< res2

-- s = Student 0 "STUDENT"
-- myMap = Map.fromList([("ab", NameField(toName 1 "abcd"))])
-- main = putStrLn(show ( asName( fromMaybe(getField_ "name" s) ) ) )

    -- conn <- connect
    --     defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "lab1"}
    -- (defs, is) <- query_ conn "CREATE TABLE IF NOT EXISTS lab1_user (id      SERIAL PRIMARY KEY,    name    VARCHAR(20) NOT NULL);"
  -- print =<<
