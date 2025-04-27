{-# LANGUAGE OverloadedStrings #-}
module Handlers.Todo (homePage, createTodo, toggleTodo, deleteTodo, TodoForm) where

import Servant
import Models (Todo(..))
import Db
import Views.Todo
import Database.Persist
import Database.Persist.Sql (toSqlKey)
import Text.Blaze.Html (Html)
import qualified Data.Text as T
import Web.FormUrlEncoded (FromForm(..), parseUnique)
import Control.Monad.IO.Class (liftIO)

data TodoForm = TodoForm { title :: T.Text }

instance FromForm TodoForm where
  fromForm f = TodoForm <$> (parseUnique "title" f)

homePage :: Handler Html
homePage = liftIO $ do
  todos <- runDB $ selectList [] []
  pure $ renderTodoList todos

createTodo :: TodoForm -> Handler Html
createTodo (TodoForm t) = liftIO $ do
  _ <- runDB $ insert (Todo t False)
  todos <- runDB $ selectList [] []
  pure $ renderTodoList todos

toggleTodo :: Int -> Handler Html
toggleTodo tid = liftIO $ do
  let key = toSqlKey (fromIntegral tid) :: Key Todo
  Just (Todo t d) <- runDB $ get key
  runDB $ replace key (Todo t (not d))
  todos <- runDB $ selectList [] []
  pure $ renderTodoList todos

deleteTodo :: Int -> Handler Html
deleteTodo tid = liftIO $ do
  let key = toSqlKey (fromIntegral tid) :: Key Todo
  runDB $ delete key
  todos <- runDB $ selectList [] []
  pure $ renderTodoList todos