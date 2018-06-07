module EmbedApp where

import Prelude hiding (apply)
import Effect (Effect)
import Node.Express.Types (Application)
import Node.Express.App (App, apply, get)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)


wowHandler :: Handler
wowHandler = send "WOW"

appSetup :: App
appSetup = do
    get "/wow" wowHandler

attach :: Application -> Effect Unit
attach = apply appSetup

foreign import realMain :: (Application -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = realMain attach
