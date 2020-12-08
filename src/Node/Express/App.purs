module Node.Express.App
    ( AppM(..)
    , App
    , HandlerFn
    , listenHttp, listenHttps, listenHostHttp, listenHostHttps
    , listenPipe, makeHttpServer, makeHttpsServer, apply
    , use, useExternal, useAt, useAtExternal, useOnParam, useOnError
    , getProp, setProp
    , http, get, post, put, delete, all

    , mkApplication, _getProp, _setProp, _http, _httpServer, _httpsServer, _listenHttp, _listenHttps
    , _listenHostHttp, _listenHostHttps, _listenPipe, _use, _useExternal, _useAt, _useAtExternal, _useOnParam, _useOnError
    ) where

import Prelude hiding (apply)

import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn4, runFn3, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign, unsafeToForeign)
import Node.Express.Handler (Handler, runHandlerM)
import Node.Express.Types (class RoutePattern, Application, Response, Request, Event, Host, Path, Port, Pipe, Method(..), Middleware)
import Node.HTTP (Server)

-- | Monad responsible for application related operations (initial setup mostly).
newtype AppM a = AppM (Application -> Effect a)
type App = AppM Unit

type HandlerFn = Request -> Response -> Effect Unit -> Effect Unit

instance functorAppM :: Functor AppM where
    map f (AppM h) = AppM \app -> liftM1 f $ h app

instance applyAppM :: Apply AppM where
    apply (AppM f) (AppM h) = AppM \app -> do
        trans <- f app
        res <- h app
        pure $ trans res

instance applicativeAppM :: Applicative AppM where
    pure x = AppM \_ -> pure x

instance bindAppM :: Bind AppM where
    bind (AppM h) f = AppM \app -> do
        res <- h app
        case f res of
             AppM g -> g app

instance monadAppM :: Monad AppM

instance monadEffectAppM :: MonadEffect AppM where
    liftEffect act = AppM \_ -> act


-- | Create a Node.HTTP server from the Express application.
-- | HTTP version
makeHttpServer :: App -> Effect Server
makeHttpServer (AppM act) = do
    app <- mkApplication
    act app
    _httpServer app

-- | Create a Node.HTTP server from the Express application.
-- | HTTPS version
makeHttpsServer :: App -> Effect Server
makeHttpsServer (AppM act) = do
    app <- mkApplication
    act app
    _httpsServer app

-- | Run application on specified port and execute callback after launch.
-- | HTTP version
listenHttp :: App -> Port -> (Event -> Effect Unit) -> Effect Server
listenHttp (AppM act) port cb = do
    app <- mkApplication
    act app
    _listenHttp app port cb

-- | Run application on specified port and execute callback after launch.
-- | HTTPS version
listenHttps :: forall opts.
    App -> Port -> opts -> (Event -> Effect Unit) -> Effect Server
listenHttps (AppM act) port opts cb = do
    app <- mkApplication
    act app
    _listenHttps app port opts cb

-- | Run application on specified port & host and execute callback after launch.
-- | HTTP version
listenHostHttp :: App -> Port -> Host -> (Event -> Effect Unit) -> Effect Server
listenHostHttp (AppM act) port host cb = do
    app <- mkApplication
    act app
    _listenHostHttp app port host cb

-- | Run application on specified port & host and execute callback after launch.
-- | HTTPS version
listenHostHttps :: forall opts.
    App -> Port -> Host-> opts -> (Event -> Effect Unit) -> Effect Server
listenHostHttps (AppM act) port host opts cb = do
    app <- mkApplication
    act app
    _listenHostHttps app port host opts cb

-- | Run application on specified named pipe and execute callback after launch.
-- | HTTP version
listenPipe :: App -> Pipe -> (Event -> Effect Unit) -> Effect Server
listenPipe (AppM act) pipe cb = do
    app <- mkApplication
    act app
    _listenPipe app pipe cb

-- | Apply App actions to existent Express.js application
apply :: App -> Application -> Effect Unit
apply (AppM act) app = act app

-- | Use specified middleware handler.
use :: Handler -> App
use middleware = AppM \app ->
    runFn2 _use app $ runHandlerM middleware

-- | Use any function as middleware.
-- | Introduced to ease usage of a bunch of external
-- | middleware written for express.js.
-- | See http://expressjs.com/4x/api.html#middleware
useExternal :: Middleware -> App
useExternal fn = AppM \app ->
    runFn2 _useExternal app fn

-- | Use specified middleware only on requests matching path.
useAt :: Path -> Handler -> App
useAt route middleware = AppM \app ->
    runFn3 _useAt app route $ runHandlerM middleware

-- | Use any function as middleware only on requests matching path.
-- | Introduced to ease usage of a bunch of external
-- | middleware written for express.js.
-- | See http://expressjs.com/4x/api.html#middleware
useAtExternal :: Path -> Middleware -> App
useAtExternal route fn = AppM \app ->
    runFn3 _useAtExternal app route fn

-- | Process route param with specified handler.
useOnParam :: String -> (String -> Handler) -> App
useOnParam param handler = AppM \app ->
    runFn3 _useOnParam app param (runHandlerM <<< handler)

-- | Use error handler. Probably this should be the last middleware to attach.
useOnError :: (Error -> Handler) -> App
useOnError handler = AppM \app ->
    runFn2 _useOnError app (runHandlerM <<< handler)


-- | Get application property.
-- | See http://expressjs.com/4x/api.html#app-settings
getProp :: forall a. String -> AppM (Maybe a)
getProp name = AppM \app ->
    liftEffect $ runFn4 _getProp app name Nothing Just

-- | Set application property.
-- | See http://expressjs.com/4x/api.html#app-settings
setProp :: forall a. String -> a -> App
setProp name val = AppM \app ->
    runFn3 _setProp app name val


-- | Bind specified handler to handle request matching route and method.
http :: forall r. (RoutePattern r) => Method -> r -> Handler -> App
http method route handler = AppM \app ->
    runFn4 _http app (show method) (unsafeToForeign route) $ runHandlerM handler

-- | Shortcut for `http GET`.
get :: forall r. (RoutePattern r) => r -> Handler -> App
get = http GET

-- | Shortcut for `http POST`.
post :: forall r. (RoutePattern r) => r -> Handler -> App
post = http POST

-- | Shortcut for `http PUT`.
put :: forall r. (RoutePattern r) => r -> Handler -> App
put = http PUT

-- | Shortcut for `http DELETE`.
delete :: forall r. (RoutePattern r) => r -> Handler -> App
delete = http DELETE

-- | Shortcut for `http ALL` (match on any http method).
all :: forall r. (RoutePattern r) => r -> Handler -> App
all = http ALL

foreign import mkApplication :: Effect Application

foreign import _getProp :: forall a. Fn4 Application String (Maybe a) (a -> Maybe a) (Effect (Maybe a))

foreign import _setProp :: forall a. Fn3 Application String a (Effect Unit)

foreign import _http :: Fn4 Application String Foreign HandlerFn (Effect Unit)

foreign import _httpServer :: Application -> Effect Server

foreign import _httpsServer :: Application -> Effect Server

foreign import _listenHttp :: Application -> Int -> (Event -> Effect Unit) -> Effect Server

foreign import _listenHttps :: forall opts. Application -> Int -> opts -> (Event -> Effect Unit) -> Effect Server

foreign import _listenHostHttp :: Application -> Int -> String -> (Event -> Effect Unit) -> Effect Server

foreign import _listenHostHttps :: forall opts. Application -> Int -> String -> opts -> (Event -> Effect Unit) -> Effect Server

foreign import _listenPipe :: Application -> String -> (Event -> Effect Unit) -> Effect Server

foreign import _use :: Fn2 Application HandlerFn (Effect Unit)

foreign import _useExternal :: Fn2 Application Middleware (Effect Unit)

foreign import _useAt :: Fn3 Application String (HandlerFn) (Effect Unit)

foreign import _useAtExternal :: Fn3 Application String Middleware (Effect Unit)

foreign import _useOnParam :: Fn3 Application String (String -> HandlerFn) (Effect Unit)

foreign import _useOnError :: Fn2 Application (Error -> HandlerFn) (Effect Unit)
