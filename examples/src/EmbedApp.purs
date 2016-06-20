module EmbedApp where

import Prelude hiding (apply)
import Control.Monad.Eff (Eff)
import Node.Express.Types (EXPRESS, ExpressM, Application)
import Node.Express.App (App, apply, get)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)


wowHandler :: forall e. Handler e
wowHandler = send "WOW"

appSetup :: forall e. App e
appSetup = do
    get "/wow" wowHandler

attach :: forall e. Application -> ExpressM e Unit
attach = apply appSetup

foreign import realMain :: forall e. (Application -> ExpressM e Unit) -> Eff (express :: EXPRESS | e) Unit

main::forall e. Eff (express::EXPRESS | e) Unit
main = realMain attach
