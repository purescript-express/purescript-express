module Main where

import Debug.Trace
import Data.Maybe
import Data.Function
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


foreign import jsonBodyParser "var jsonBodyParser = require('body-parser').json()"
    :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

indexHandler :: Handler
indexHandler = send "Make POST request with JSON body like {\"message\": <msg>} to get your message back"

echoHandler :: Handler
echoHandler = do
    messageParam <- getBodyParam "message"
    case messageParam of
        Nothing -> send "You did not say anything"
        Just message -> send $ "You said: " ++ message

appSetup :: App
appSetup = do
    useExternal jsonBodyParser
    get  "/" indexHandler
    post "/" echoHandler

main = do
    listenHttp appSetup 8080 \_ -> trace $ "Listening on 8080"
