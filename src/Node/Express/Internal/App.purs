module Node.Express.Internal.App where

import Data.Function
import Data.Foreign
import Data.Foreign.EasyFFI
import Data.Maybe
import Data.String.Regex
import Control.Monad.Eff
import Node.Express.Types
import Node.Express.Internal.Utils


foreign import express "var express = require('express')" :: Unit

foreign import intlMkApplication
    "function intlMkApplication() {\
    \    return express();\
    \}"
    :: ExpressM Application


intlAppGetProp ::
    forall a. (ReadForeign a) =>
    Application -> String -> ExpressM (Maybe a)
intlAppGetProp app name = do
    let getter :: Application -> String -> ExpressM Foreign
        getter = unsafeForeignFunction ["a", "n", ""] "a.get(n)"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter app name)

intlAppSetProp ::
    forall a.
    Application -> String -> a -> ExpressM Unit
intlAppSetProp = unsafeForeignProcedure ["app", "name", "val", ""]
    "app.set(name, val)"

class Route a
instance routeString :: Route String
instance routeRegex  :: Route Regex
type HandlerFn = Request -> Response -> ExpressM Unit -> ExpressM Unit

intlAppHttp ::
    forall r. (Route r) =>
    Application -> String -> r -> HandlerFn -> ExpressM Unit
intlAppHttp = unsafeForeignProcedure ["app", "method", "route", "cb", ""]
    "app[method](route, function(req, resp, next) { cb(req)(resp)(next)(); })"

intlAppListen ::
    forall e.
    Application -> Number -> (Event -> Eff e Unit) -> ExpressM Unit
intlAppListen = unsafeForeignProcedure ["app", "port", "cb", ""]
    "app.listen(port, function(e) { cb(e)(); });"

intlAppUse ::
    Application -> HandlerFn -> ExpressM Unit
intlAppUse = unsafeForeignProcedure ["app", "mw", ""]
    "app.use(function(req, resp, next) { mw(req)(resp)(next)(); });"

-- TODO: engine, param, route, locals
