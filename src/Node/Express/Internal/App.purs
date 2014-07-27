module Node.Express.Internal.App where

import Data.Function
import Data.Foreign
import Data.Foreign.EasyFFI
import Data.Maybe
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
        getter = unsafeForeignFunction ["app", "name", ""] "app.get(name)"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter app name)

intlAppSetProp ::
    forall a. (ReadForeign a) =>
    Application -> String -> a -> ExpressM Unit
intlAppSetProp = unsafeForeignProcedure ["app", "name", "val", ""]
    "app.set(name, val)"


type HandlerFn = Request -> Response -> ExpressM Unit -> ExpressM Unit

intlAppHttp ::
    forall r. (Route r) =>
    Application -> String -> r -> HandlerFn -> ExpressM Unit
intlAppHttp = unsafeForeignProcedure ["app", "method", "route", "cb", ""]
    "app[method](route, function(req, resp, next) { return cb(req)(resp)(next)(); })"

intlAppListen ::
    forall e.
    Application -> Number -> (Event -> Eff e Unit) -> ExpressM Unit
intlAppListen = unsafeForeignProcedure ["app", "port", "cb", ""]
    "app.listen(port, function(e) { return cb(e)(); });"


intlAppUse ::
    Application -> HandlerFn -> ExpressM Unit
intlAppUse = unsafeForeignProcedure ["app", "mw", ""]
    "app.use(function(req, resp, next) { return mw(req)(resp)(next)(); });"

intlAppUseAt ::
    Application -> String -> HandlerFn -> ExpressM Unit
intlAppUseAt = unsafeForeignProcedure ["app", "route", "mw", ""]
    "app.use(route, function(req, resp, next) { return mw(req)(resp)(next)(); });"

intlAppUseOnParam ::
    Application -> String -> (String -> HandlerFn) -> ExpressM Unit
intlAppUseOnParam = unsafeForeignFunction ["app", "name", "cb", ""]
    "app.param(name, function(req, resp, next, val) { return cb(val)(req)(resp)(next)(); })"

