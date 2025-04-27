{-# LANGUAGE OverloadedStrings #-}

module Views.Todo (renderTodoItem, renderTodoList) where

import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Database.Persist
import Models (Todo(..))
import Control.Monad (forM_)
import Database.Persist.Sql (fromSqlKey)

hxPost :: AttributeValue -> Attribute
hxPost = customAttribute "hx-post"

hxDelete :: AttributeValue -> Attribute
hxDelete = customAttribute "hx-delete"

hxTarget :: AttributeValue -> Attribute
hxTarget = customAttribute "hx-target"

hxSwap :: AttributeValue -> Attribute
hxSwap = customAttribute "hx-swap"

renderTodoList :: [Entity Todo] -> Html
renderTodoList todos = docTypeHtml $ do
  H.head $ do
    H.title "Todo List"
    H.script ! A.src "https://unpkg.com/htmx.org@1.9.2" $ mempty 
  H.body ! A.id "todo-list" $ do
    H.h1 "HTMX Todo List"
    H.form 
      ! hxPost "/todos" 
      ! hxTarget "#todo-list" 
      ! hxSwap "outerHTML" $ do
        H.input ! A.type_ "text" ! A.name "title"
        H.button "Add"
    H.div $
      H.ul $ forM_ todos renderTodoItem

renderTodoItem :: Entity Todo -> Html
renderTodoItem (Entity tid (Todo todoText done)) = 
  H.li $ do
    H.input 
      ! A.type_ "checkbox"
      ! A.name "done"
      ! hxPost (H.toValue $ "/todos/" ++ show (fromSqlKey tid) ++ "/toggle")
      ! hxTarget "#todo-list"
      ! hxSwap "outerHTML"
      ! (if done then A.checked "checked" else mempty)
    H.toHtml (" " <> todoText)
    H.form 
      ! hxDelete (H.toValue $ "/todos/" ++ show (fromSqlKey tid)) 
      ! hxTarget "#todo-list" 
      ! hxSwap "outerHTML" $ do
        H.button "Delete"

