module Main where

import Network.Wai.Handler.Warp (run)
import Lib
import Db (doMigrations)

main :: IO ()
main = do
  putStrLn "Running migrations..."
  doMigrations
  putStrLn "Starting app on http://localhost:8080"
  run 8080 app
