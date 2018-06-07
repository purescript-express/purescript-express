module JSMiddleware where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn3)
import Effect (Effect)
import Effect.Console (log)
import Node.Express.Types (Response, Request)
import Node.Express.App (App, listenHttp, post, get, useExternal)
import Node.Express.Handler (Handler)
import Node.Express.Request (getBodyParam)
import Node.Express.Response (send)
import Node.HTTP (Server)


foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

indexHandler :: Handler
indexHandler = send "Make POST request with JSON body like {\"message\": <msg>} to get your message back"

echoHandler :: Handler
echoHandler = do
    messageParam <- getBodyParam "message"
    case messageParam of
        Nothing -> send "You did not say anything"
        Just message -> send $ "You said: " <> message

appSetup :: App
appSetup = do
    useExternal jsonBodyParser
    get  "/" indexHandler
    post "/" echoHandler

main :: Effect Server
main = do
    listenHttp appSetup 8080 \_ -> log $ "Listening on 8080"
