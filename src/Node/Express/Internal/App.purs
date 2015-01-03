-- Patience you must have, my young padawan. This module leave you must --
module Node.Express.Internal.App where

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


foreign import intlMkApplication
    """
    function intlMkApplication() {
        var express = module.require('express');
        return express();
    }
    """
    :: ExpressM Application

intlAppGetProp ::
    forall a. (IsForeign a) =>
    Application -> String -> ExpressM (Maybe a)
intlAppGetProp app name = do
    let getter :: Application -> String -> ExpressM Foreign
        getter = unsafeForeignFunction ["app", "name", ""] "app.get(name)"
    liftM1 (eitherToMaybe <<< read) (getter app name)

intlAppSetProp ::
    forall a. (IsForeign a) =>
    Application -> String -> a -> ExpressM Unit
intlAppSetProp = unsafeForeignProcedure ["app", "name", "val", ""]
    "app.set(name, val)"


type HandlerFn = Request -> Response -> ExpressM Unit -> ExpressM Unit

intlAppHttp ::
    forall r. (RoutePattern r) =>
    Application -> String -> r -> HandlerFn -> ExpressM Unit
intlAppHttp = unsafeForeignProcedure ["app", "method", "route", "cb", ""]
    "app[method](route, function(req, resp, next) { return cb(req)(resp)(next)(); })"

foreign import intlAppListenHttp
    """
    function intlAppListenHttp(app) {
        return function(port) {
            return function(cb) {
                return function() {
                    var http = module.require('http');
                    http.createServer(app).listen(port, function(e) {
                        return cb(e)();
                    });
                }
            }
        }
    }
    """::
    forall e.
    Application -> Number -> (Event -> Eff e Unit) -> ExpressM Unit

foreign import intlAppListenHttps
    """
    function intlAppListenHttps(app) {
        return function(port) {
            return function(opts) {
                return function(cb) {
                    return function() {
                        var https = module.require('https');
                        https.createServer(opts, app).listen(port, function(e) {
                            return cb(e)();
                        });
                    }
                }
            }
        }
    }
    """::
    forall opts e.
    Application -> Number -> opts -> (Event -> Eff e Unit) -> ExpressM Unit


intlAppUse ::
    Application -> HandlerFn -> ExpressM Unit
intlAppUse = unsafeForeignProcedure ["app", "mw", ""]
    "app.use(function(req, resp, next) { return mw(req)(resp)(next)(); });"

intlAppUseExternal ::
    Application -> Fn3 Request Response (ExpressM Unit) (ExpressM Unit) -> ExpressM Unit
intlAppUseExternal = unsafeForeignProcedure ["app", "mw", ""] "app.use(mw);"

intlAppUseAt ::
    Application -> String -> HandlerFn -> ExpressM Unit
intlAppUseAt = unsafeForeignProcedure ["app", "route", "mw", ""]
    "app.use(route, function(req, resp, next) { return mw(req)(resp)(next)(); });"

intlAppUseOnParam ::
    Application -> String -> (String -> HandlerFn) -> ExpressM Unit
intlAppUseOnParam = unsafeForeignProcedure ["app", "name", "cb", ""]
    "app.param(name, function(req, resp, next, val) { return cb(val)(req)(resp)(next)(); })"

intlAppUseOnError ::
    Application -> (Error -> HandlerFn) -> ExpressM Unit
intlAppUseOnError = unsafeForeignProcedure ["app", "cb", ""]
    "app.use(function(err, req, resp, next) { return cb(err)(req)(resp)(next)(); })"

