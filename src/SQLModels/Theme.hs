{-# LANGUAGE OverloadedStrings #-}

module SQLModels.Theme where

import SQLModel
import Utils

import Data.List
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

-- data Theme = MakeTheme { themeId :: Integer, themeName :: String }
--
-- makeTheme :: Integer -> String -> Theme
-- makeTheme id name | length name > 120 = error "can't make"
--                   | otherwise = MakeTheme id name
--
-- setThemeName :: Theme -> String -> Theme
-- setThemeName theme = makeTheme (themeId theme)
--
-- instance SQLModel Theme where
--
--   get conn themeId = do
--     s <- prepareStmt conn "SELECT * FROM `themes` where themes.id = ?"
--     (defs, is) <- queryStmt conn s $ toMySQLParams [themeId]
--     rows <- Streams.toList is
--     let selected = last (map unpack rows)
--     return $ makeTheme (sel1 selected) (sel2 selected)
--
--   update conn theme = do
--      execute conn q [toMySQL $ themeName theme, toMySQL $ themeId theme]
--      where
--        q = "UPDATE `themes` SET name = ? where themes.id = ?;"
--
--   create conn theme = do
--      execute conn q [toMySQL $ themeName theme]
--      where
--        q = "insert into themes (name) values (?)"
--
--   fetchAll conn = do
--     s <- prepareStmt conn "SELECT * FROM 'themes'"
--     (defs, is) <- queryStmt conn s
--     rows <- Streams.toList is
--     let unpackedRows = map unpack rows
--     return zipWith makeTheme (map sel1 unpackedRows) (map sel2 unpackedRows)
