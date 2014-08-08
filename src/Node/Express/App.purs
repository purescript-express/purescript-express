module Node.Express.App
    ( AppM()
    , App()
    , listen, use, useExternal, useAt, useOnParam, useOnError
    , getProp, setProp
    , http, get, post, put, delete, all
    ) where

import Data.Foreign
import Data.Function
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Class

import Node.Express.Types
import Node.Express.Internal.App
import Node.Express.Handler


--| Monad responsible for application related operations (initial setup mostly).
data AppM a = AppM (Application -> ExpressM a)
type App = AppM Unit

instance functorAppM :: Functor AppM where
    (<$>) f (AppM h) = AppM \app -> liftM1 f $ h app

instance applyAppM :: Apply AppM where
    (<*>) (AppM f) (AppM h) = AppM \app -> do
        res <- h app
        trans <- f app
        return $ trans res

instance applicativeAppM :: Applicative AppM where
    pure x = AppM \_ -> return x

instance bindAppM :: Bind AppM where
    (>>=) (AppM h) f = AppM \app -> do
        res <- h app
        case f res of
             AppM g -> g app

instance monadAppM :: Monad AppM

instance monadEffAppM :: MonadEff AppM where
    liftEff act = AppM \_ -> liftEff act


--| Run application on specified port and execute callback after launch.
listen :: forall e. App -> Port -> (Event -> Eff e Unit) -> ExpressM Unit
listen (AppM act) port cb = do
    pure express -- DO NOT DELETE THIS LINE
    -- Otherwise call to 'require(express)' may be optimized out
    app <- intlMkApplication
    act app
    intlAppListen app port cb

--| Use specified middleware handler.
use :: Handler -> App
use middleware = AppM \app ->
    intlAppUse app $ withHandler middleware

--| Use any function as middleware.
--  Introduced to ease usage of a bunch of external
--  middleware written for express.js.
--  See http://expressjs.com/4x/api.html#middleware
useExternal :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit) -> App
useExternal fn = AppM \app ->
    intlAppUseExternal app fn

--| Use specified middleware only on requests matching path.
useAt :: Path -> Handler -> App
useAt route middleware = AppM \app ->
    intlAppUseAt app route $ withHandler middleware

--| Process route param with specified handler.
useOnParam :: String -> (String -> Handler) -> App
useOnParam param handler = AppM \app ->
    intlAppUseOnParam app param (withHandler <<< handler)

--| Use error handler. Probably this should be the last middleware to attach.
useOnError :: (Error -> Handler) -> App
useOnError handler = AppM \app ->
    intlAppUseOnError app (withHandler <<< handler)


--| Get application property.
-- See http://expressjs.com/4x/api.html#app-settings
getProp :: forall a. (ReadForeign a) => String -> AppM (Maybe a)
getProp name = AppM \app ->
    intlAppGetProp app name

--| Set application property.
-- See http://expressjs.com/4x/api.html#app-settings
setProp :: forall a. (ReadForeign a) => String -> a -> App
setProp name val = AppM \app ->
    intlAppSetProp app name val


--| Bind specified handler to handle request matching route and method.
http :: forall r. (RoutePattern r) => Method -> r -> Handler -> App
http method route handler = AppM \app ->
    intlAppHttp app (show method) route $ withHandler handler

--| Shortcut for `http GET`.
get :: forall r. (RoutePattern r) => r -> Handler -> App
get = http GET

--| Shortcut for `http POST`.
post :: forall r. (RoutePattern r) => r -> Handler -> App
post = http POST

--| Shortcut for `http PUT`.
put :: forall r. (RoutePattern r) => r -> Handler -> App
put = http PUT

--| Shortcut for `http DELETE`.
delete :: forall r. (RoutePattern r) => r -> Handler -> App
delete = http DELETE

--| Shortcut for `http ALL` (match on any http method).
all :: forall r. (RoutePattern r) => r -> Handler -> App
all = http ALL

