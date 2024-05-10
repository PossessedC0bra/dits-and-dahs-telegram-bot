module Main (main) where

import API (app)
import AWS.Lambda.Runtime (mRuntime)
import Network.Wai.Handler.Hal (run)

main :: IO ()
main = mRuntime $ run app
