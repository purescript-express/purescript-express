module JSMiddleware where

import Prelude hiding (apply)
import Data.Maybe
import Data.Function
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log)
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import Node.Express.Request
import Node.Express.Response
import Node.HTTP (Server())


foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

indexHandler :: forall e. Handler e
indexHandler = send "Make POST request with JSON body like {\"message\": <msg>} to get your message back"

echoHandler :: forall e. Handler e
echoHandler = do
    messageParam <- getBodyParam "message"
    case messageParam of
        Nothing -> send "You did not say anything"
        Just message -> send $ "You said: " ++ message

appSetup :: forall e. App e
appSetup = do
    useExternal jsonBodyParser
    get  "/" indexHandler
    post "/" echoHandler

main :: forall e. Eff (express :: EXPRESS, console :: CONSOLE | e) Server
main = do
    listenHttp appSetup 8080 \_ -> log $ "Listening on 8080"
