{-# LANGUAGE OverloadedStrings #-}

module Db (runDB, doMigrations, connStr) where

import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Database.Persist.Postgresql (withPostgresqlConn, ConnectionString)
import Database.Persist.Sql (runSqlConn, SqlPersistT, runMigration)
import Models (migrateAll)
import System.Environment (getEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.ByteString.Char8 (pack)

connStr :: IO ConnectionString
connStr = do
  _ <- loadFile defaultConfig
  host <- getEnv "DB_HOST"
  dbname <- getEnv "DB_NAME"
  user <- getEnv "DB_USER"
  password <- getEnv "DB_PASSWORD"
  port <- getEnv "DB_PORT"
  let conn = "host=" ++ host ++ " dbname=" ++ dbname ++ " user=" ++ user ++ " password=" ++ password ++ " port=" ++ port
  return (pack conn)

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB action = do
  conn <- connStr
  runNoLoggingT $ runResourceT $
    withPostgresqlConn conn $ \backend ->
      runSqlConn action backend

doMigrations :: IO ()
doMigrations = runDB $ runMigration migrateAll

