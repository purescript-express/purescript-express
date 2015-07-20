-- Patience you must have, my young padawan. This module leave you must --
module Node.Express.Internal.App where

import Prelude
import Data.Function
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.EasyFFI
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Node.Express.Types
import Node.Express.Internal.Utils
import Node.Express.Handler


foreign import intlMkApplication :: forall e. ExpressM e Application

intlAppGetProp ::
    forall e a. (IsForeign a) =>
    Application -> String -> ExpressM e (Maybe a)
intlAppGetProp app name = do
    let getter :: Application -> String -> ExpressM e Foreign
        getter = unsafeForeignFunction ["app", "name", ""] "app.get(name)"
    liftM1 (eitherToMaybe <<< read) (getter app name)

intlAppSetProp ::
    forall e a. (IsForeign a) =>
    Application -> String -> a -> ExpressM e Unit
intlAppSetProp = unsafeForeignProcedure ["app", "name", "val", ""]
    "app.set(name, val)"


type HandlerFn e = Request -> Response -> Eff (express :: EXPRESS | e) Unit -> Eff (express :: EXPRESS | e) Unit

foreign import intlAppHttp ::
    forall e r. (RoutePattern r) =>
    Fn4 Application String r (HandlerFn e) (Eff (express :: EXPRESS | e) Unit)

foreign import intlAppListenHttp :: forall e.
    Application -> Int -> (Event -> Eff e Unit) -> ExpressM e Unit

foreign import intlAppListenHttps :: forall opts e.
    Application -> Int -> opts -> (Event -> Eff e Unit) -> ExpressM e Unit

foreign import intlAppUse ::
    forall e. Fn2 Application (HandlerFn e) (Eff (express :: EXPRESS | e) Unit)

intlAppUseExternal ::
    forall e. Application -> Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit) -> ExpressM e Unit
intlAppUseExternal = unsafeForeignProcedure ["app", "mw", ""] "app.use(mw);"

foreign import intlAppUseAt ::
    forall e. Fn3 Application String (HandlerFn e) (Eff (express :: EXPRESS | e) Unit)

foreign import intlAppUseOnParam ::
    forall e. Fn3 Application String (String -> HandlerFn e) (Eff (express :: EXPRESS | e) Unit)

foreign import intlAppUseOnError ::
    forall e. Fn2 Application (Error -> HandlerFn e) (Eff (express :: EXPRESS | e) Unit)

