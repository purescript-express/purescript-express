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


type HandlerFn e = Request -> Response -> ExpressM e Unit -> ExpressM e Unit

intlAppHttp ::
    forall e r. (RoutePattern r) =>
    Application -> String -> r -> HandlerFn e -> ExpressM e Unit
intlAppHttp = unsafeForeignProcedure ["app", "method", "route", "cb", ""]
    "app[method](route, function(req, resp, next) { return cb(req)(resp)(next)(); })"

foreign import intlAppListenHttp :: forall e.
    Application -> Int -> (Event -> Eff e Unit) -> ExpressM e Unit

foreign import intlAppListenHttps :: forall opts e.
    Application -> Int -> opts -> (Event -> Eff e Unit) -> ExpressM e Unit


intlAppUse ::
    forall e. Application -> HandlerFn e -> ExpressM e Unit
intlAppUse = unsafeForeignProcedure ["app", "mw", ""]
    "app.use(function(req, resp, next) { return mw(req)(resp)(next)(); });"

intlAppUseExternal ::
    forall e. Application -> Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit) -> ExpressM e Unit
intlAppUseExternal = unsafeForeignProcedure ["app", "mw", ""] "app.use(mw);"

intlAppUseAt ::
    forall e. Application -> String -> HandlerFn e -> ExpressM e Unit
intlAppUseAt = unsafeForeignProcedure ["app", "route", "mw", ""]
    "app.use(route, function(req, resp, next) { return mw(req)(resp)(next)(); });"

intlAppUseOnParam ::
    forall e. Application -> String -> (String -> HandlerFn e) -> ExpressM e Unit
intlAppUseOnParam = unsafeForeignProcedure ["app", "name", "cb", ""]
    "app.param(name, function(req, resp, next, val) { return cb(val)(req)(resp)(next)(); })"

intlAppUseOnError ::
    forall e. Application -> (Error -> HandlerFn e) -> ExpressM e Unit
intlAppUseOnError = unsafeForeignProcedure ["app", "cb", ""]
    "app.use(function(err, req, resp, next) { return cb(err)(req)(resp)(next)(); })"

