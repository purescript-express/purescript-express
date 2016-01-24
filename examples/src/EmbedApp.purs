module EmbedApp where

import Prelude hiding (apply)
import Control.Monad.Eff
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import Node.Express.Response


wowHandler :: forall e. Handler e
wowHandler = send "WOW"

appSetup :: forall e. App e
appSetup = do
    get "/wow" wowHandler

attach :: forall e. Application -> ExpressM e Unit
attach = apply appSetup

foreign import realMain :: forall e. (Application -> ExpressM e Unit) -> Eff (express :: EXPRESS | e) Unit

main = realMain attach
