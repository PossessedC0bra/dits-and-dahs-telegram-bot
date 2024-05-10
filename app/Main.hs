module Main (main) where

import API (app)
import Configuration.Dotenv as Dotenv (defaultConfig, loadFile)
import Network.Wai.Handler.Warp (runEnv)

main :: IO ()
main = do
  Dotenv.loadFile defaultConfig
  -- BS.putStr . encode $ BotCommand
  putStrLn "Starting server on localhost:8080"
  runEnv 8080 app
