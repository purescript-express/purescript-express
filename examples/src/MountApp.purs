module MountApp where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App, listenHttp, mount)
import Node.Express.Types (Application)
import Node.HTTP (Server)

foreign import thirdPartMain :: Application -> Effect Unit

app :: App
app = mount thirdPartMain

main :: Effect Server
main = do
    listenHttp app 8080 \_ ->
        log $ "Listening on " <> show 8080
