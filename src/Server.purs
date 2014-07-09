module Server where

import Debug.Trace
import Data.Either
import Data.Foreign.EasyFFI
import Control.Monad.Trans
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


indexHandler :: Handler
indexHandler = do
    lift (liftEff $ trace "Answering")
    json { test: "OK" }

logger :: ExpressM Unit -> Handler
logger next = do
    lift (liftEff (trace "Got request") >>= \_ -> next)

appSetup :: App
appSetup = do
    use logger
    get "/" indexHandler

main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listen appSetup port \_ ->
        trace $ "Listening on " ++ show port
