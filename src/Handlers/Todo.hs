{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handlers.Todo (homePage, createTodo, toggleTodo, deleteTodo, TodoForm) where

import Servant
import Models (Todo(..))
import Db
import Views.Todo
import Database.Persist.Sql
    ( rawSql, rawExecute, Entity, PersistValue( PersistInt64, PersistText ) )
import Text.Blaze.Html (Html)
import qualified Data.Text as T
import Web.FormUrlEncoded (FromForm(..), parseUnique)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

data TodoForm = TodoForm { title :: T.Text }

instance FromForm TodoForm where
  fromForm f = TodoForm <$> parseUnique "title" f

homePage :: Handler Html
homePage = liftIO $ do
  todos <- runDB $
    rawSql "SELECT ?? FROM todo ORDER BY id DESC" []
      :: IO [Entity Todo]
  pure $ renderTodoList todos

createTodo :: TodoForm -> Handler Html
createTodo (TodoForm t) = liftIO $ do
  _ <- runDB $
    rawExecute
      "INSERT INTO todo (title, done) VALUES (?, FALSE)"
      [PersistText t]
  todos <- runDB $
    rawSql "SELECT ?? FROM todo ORDER BY id DESC" []
  pure $ renderTodoList todos

toggleTodo :: Int -> Handler Html
toggleTodo tid = liftIO $ do
  void $ runDB $
    rawExecute
      "UPDATE todo SET done = NOT done WHERE id = ?"
      [PersistInt64 (fromIntegral tid)]
  todos <- runDB $
    rawSql "SELECT ?? FROM todo ORDER BY id DESC" []
  pure $ renderTodoList todos

deleteTodo :: Int -> Handler Html
deleteTodo tid = liftIO $ do
  void $ runDB $
    rawExecute
      "DELETE FROM todo WHERE id = ?"
      [PersistInt64 (fromIntegral tid)]
  todos <- runDB $
    rawSql "SELECT ?? FROM todo ORDER BY id DESC" []
  pure $ renderTodoList todos
