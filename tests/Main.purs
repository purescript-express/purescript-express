module Main where

import Debug.Trace
import Data.Either
import Data.Foreign.EasyFFI
import Control.Monad.Trans
import Control.Monad.Eff.Class
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


indexHandler :: Handler
indexHandler = do
    liftEff $ trace "Answering"
    json { test: "OK" }

logger1 :: ExpressM Unit -> Handler
logger1 next = do
    liftEff $ trace "Logger 1 got request"
    liftEff next

logger2 :: ExpressM Unit -> Handler
logger2 next = do
    liftEff $ trace "Logger 2 got request"
    liftEff next

appSetup :: App
appSetup = do
    liftEff $ trace "Setting up"
    use logger1
    use logger2
    get "/" indexHandler

main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listen appSetup port \_ ->
        trace $ "Listening on " ++ show port
