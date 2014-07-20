module Main where

import Debug.Trace
import Data.String.Regex
import Data.Foreign.EasyFFI
import Control.Monad.Eff.Class
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


globalHandler :: Handler
globalHandler = do
    liftEff $ trace "Answering..."
    next

indexHandler :: Handler
indexHandler = do
    send "index"

regexParamHandler :: Handler
regexParamHandler = do
    p1 <- params 0
    liftEff $ print p1
    send "regex"

namedParamHandler :: Handler
namedParamHandler = do
    pn <- params "name"
    liftEff $ print pn
    send "named"

logger1 :: Handler
logger1 = do
    liftEff $ trace "Logger 1 got request"
    next

logger2 :: Handler
logger2 = do
    liftEff $ trace "Logger 2 got request"
    next

appSetup :: App
appSetup = do
    liftEff $ trace "Setting up"
    use logger1
    use logger2
    all "*" globalHandler
    get (regex "/([0-9]+)" "") regexParamHandler
    get "/:name" namedParamHandler
    get "/" indexHandler

main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listen appSetup port \_ ->
        trace $ "Listening on " ++ show port
