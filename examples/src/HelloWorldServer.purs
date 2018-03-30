module HelloWorldServer where

import Prelude hiding (apply)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Express.App (App, listenHttp, get)
import Node.Express.Response (send)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)

app :: forall e. App (console :: CONSOLE | e)
app = get "/" $ send "Hello, World!"

main :: forall e. Eff (express :: EXPRESS, console :: CONSOLE | e) Server
main = do
    listenHttp app 8080 \_ ->
        log $ "Listening on " <> show 8080