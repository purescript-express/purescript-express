module EmbedApp where

import Prelude hiding (apply)
import Control.Monad.Eff
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


wowHandler :: Handler
wowHandler = send "WOW"

appSetup :: App
appSetup = do
    get "/wow" wowHandler

attach :: Application -> ExpressM Unit
attach = apply appSetup

foreign import realMain :: forall e. (Application -> ExpressM Unit) -> Eff e Unit

main = realMain attach
