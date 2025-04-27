{-# LANGUAGE DataKinds, TypeOperators #-}

module Lib (app) where

import Servant
import Handlers.Todo
import Text.Blaze.Html (Html)
import Servant.HTML.Blaze (HTML)

type API =
       Get '[HTML] Html
  :<|> "todos" :> ReqBody '[FormUrlEncoded] TodoForm :> Post '[HTML] Html
  :<|> "todos" :> Capture "todoId" Int :> "toggle" :> Post '[HTML] Html
  :<|> "todos" :> Capture "todoId" Int :> Delete '[HTML] Html

apiProxy :: Proxy API
apiProxy = Proxy

app :: Application
app = serve apiProxy server

server :: Server API
server = homePage :<|> createTodo :<|> toggleTodo :<|> deleteTodo
