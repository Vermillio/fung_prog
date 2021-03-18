{-# LANGUAGE OverloadedStrings #-}

module SQLModels.ThemeSource where

import SQLModels.SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

data ThemeSource = MakeThemeSource { themeSourceId :: Integer, themeSourceThemeId :: Integer, themeSourceLink :: String }

makeThemeSource :: Integer -> Integer-> String -> ThemeSource
makeThemeSource id themeId link | length link > 200 = error "can't make"
                               | otherwise = MakeThemeSource id themeId link

setThemeSourceThemeId :: ThemeSource -> Integer -> ThemeSource
setThemeSourceThemeId themeSource themeId = makeThemeSource (themeSourceId themeSource) themeId (themeSourceLink themeSource)

setThemeSourceLink :: ThemeSource -> String -> ThemeSource
setThemeSourceLink themeSource link = makeThemeSource (themeSourceId themeSource) (themeSourceThemeId themeSource) link

-- instance SQLModels.SQLModel ThemeSource where
--
--   get conn themeSourceId = do
--     s <- prepareStmt conn "SELECT * FROM `themeSources` where themeSources.id = ?"
--     (defs, is) <- queryStmt conn s $ toMySQLParams [themeSourceId]
--     rows <- Streams.toList is
--     let selected = last (map unpack rows)
--     return $ makeThemeSource (sel1 selected) (sel2 selected) (sel3 selected)
--
--   update conn themesrc = do
--      execute conn q [toMySQL $ themeSourceThemeId themesrc, toMySQL $ themeSourceLink themesrc, toMySQL $ themeSourceId themesrc]
--      where
--        q = "UPDATE `themeSources` SET theme_id = ?, link = ? where themeSources.id = ?;"
--
--   create conn themesrc = do
--      execute conn q [toMySQL $ themeSourceThemeId themesrc, toMySQL $ themeSourceLink themesrc]
--      where
--        q = "insert into themeSources (theme_id, link) values (?, ?)"
--
--   fetchAll conn = do
--     s <- prepareStmt conn "SELECT * FROM 'theme_sources'"
--     (defs, is) <- queryStmt conn s $ toMySQLParams[]
--     rows <- Streams.toList is
--     let unpackedRows = map unpack rows
--     return $ zipWith3 makeThemeSource (map sel1 unpackedRows) (map sel2 unpackedRows) (map sel3 unpackedRows)
