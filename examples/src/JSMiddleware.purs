module JSMiddleware where

import Prelude hiding (apply)
import Data.Maybe
import Data.Function
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log)
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import Node.HTTP (Server())


foreign import jsonBodyParser :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

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

main :: forall e. Eff (express :: Express | e) Server
main = do
    listenHttp appSetup 8080 \_ -> log $ "Listening on 8080"
