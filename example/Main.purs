module Main where

import Debug.Trace
import Data.String.Regex
import Data.Foreign.EasyFFI
import Data.Maybe
import Control.Monad.Eff.Class
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


globalHandler :: Handler
globalHandler = do
    getUrl >>= (liftEff <<< trace)
    getOriginalUrl >>= (liftEff <<< trace)
    liftEff $ trace "Answering..."
    next

indexHandler :: Handler
indexHandler = do
    vals <- getQueryParams "params"
    liftEff $ print vals
    send "index"

regexParamHandler :: Handler
regexParamHandler = do
    p1 <- getRouteParam 0
    liftEff $ print p1
    userKnown <- (getUserData "userKnown") :: HandlerM (Maybe Boolean)
    liftEff $ print userKnown
    send "regex"

namedParamHandler :: Handler
namedParamHandler = do
    getRoute >>= (liftEff <<< trace)
    (getRouteParam "name") >>= (liftEff <<< print)
    userKnown <- (getUserData "userKnown") :: HandlerM (Maybe Boolean)
    liftEff $ print userKnown
    send "named"

routeParamProcessor :: String -> Handler
routeParamProcessor value = do
    putUserData "userKnown" (value == "me")
    next

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
    useAt "/123" logger2
    useOnParam "name" routeParamProcessor
    all "*" globalHandler
    get (regex "/([0-9]+)" $ parseFlags "") regexParamHandler
    get "/tests.js" (sendFile "tmp/tests.js")
    get "/:name" namedParamHandler
    get "/" indexHandler

main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listen appSetup port \_ ->
        trace $ "Listening on " ++ show port
