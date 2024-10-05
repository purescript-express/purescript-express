module Node.Express.App
  ( AppM(..)
  , App
  , listenHttp
  , listenHttps
  , listenHostHttp
  , listenHostHttps
  , listenPipe
  , makeHttpServer
  , makeHttpsServer
  , apply
  , use
  , useExternal
  , useAt
  , useAtExternal
  , param
  , useOnError
  , getProp
  , setProp
  , http
  , get
  , post
  , put
  , delete
  , all
  , mkApplication
  , _getProp
  , _setProp
  , _http
  , _httpServer
  , _httpsServer
  , _listenHttp
  , _listenHttps
  , _listenHostHttp
  , _listenHostHttps
  , _listenPipe
  , _use
  , _useAt
  , _param
  , _useOnError
  ) where

import Node.Express.Types (class RoutePattern, Application, Event, HandlerFnInternal_Err_Req_Res_Next, HandlerFnInternal_Req_Res_Next, HandlerFnInternal_Req_Res_Next_Param, Host, Method(..), Middleware, Path, Pipe, Port)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, liftM1, map, pure, show, ($))

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff.Compat (runEffectFn1)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Foreign (Foreign, unsafeToForeign)
import Node.Express.Handler (Handler, runErrorHandlerM, runHandlerM, runParamHandlerM)
import Node.HTTP.Types (HttpServer, HttpsServer)
import Unsafe.Coerce (unsafeCoerce)

-- | Monad responsible for application related operations (initial setup mostly).
newtype AppM a = AppM (Application -> Effect a)
type App = AppM Unit

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
makeHttpServer :: App -> Effect HttpServer
makeHttpServer (AppM act) = do
  app <- mkApplication
  act app
  runEffectFn1 _httpServer app

-- | Create a Node.HTTP server from the Express application.
-- | HTTPS version
makeHttpsServer :: App -> Effect HttpsServer
makeHttpsServer (AppM act) = do
  app <- mkApplication
  act app
  runEffectFn1 _httpsServer app

-- | Run application on specified port and execute callback after launch.
-- | HTTP version
listenHttp :: App -> Port -> (Event -> Effect Unit) -> Effect HttpServer
listenHttp (AppM act) port cb = do
  app <- mkApplication
  act app
  runEffectFn3 _listenHttp app port (mkEffectFn1 cb)

-- | Run application on specified port and execute callback after launch.
-- | HTTPS version
listenHttps
  :: forall opts
   . App
  -> Port
  -> opts
  -> (Event -> Effect Unit)
  -> Effect HttpsServer
listenHttps (AppM act) port opts cb = do
  app <- mkApplication
  act app
  runEffectFn4 _listenHttps app port opts (mkEffectFn1 cb)

-- | Run application on specified port & host and execute callback after launch.
-- | HTTP version
listenHostHttp :: App -> Port -> Host -> (Event -> Effect Unit) -> Effect HttpServer
listenHostHttp (AppM act) port host cb = do
  app <- mkApplication
  act app
  runEffectFn4 _listenHostHttp app port host (mkEffectFn1 cb)

-- | Run application on specified port & host and execute callback after launch.
-- | HTTPS version
listenHostHttps
  :: forall opts
   . App
  -> Port
  -> Host
  -> opts
  -> (Event -> Effect Unit)
  -> Effect HttpsServer
listenHostHttps (AppM act) port host opts cb = do
  app <- mkApplication
  act app
  runEffectFn5 _listenHostHttps app port host opts (mkEffectFn1 cb)

-- | Run application on specified named pipe and execute callback after launch.
-- | HTTP version
listenPipe :: App -> Pipe -> (Event -> Effect Unit) -> Effect HttpServer
listenPipe (AppM act) pipe cb = do
  app <- mkApplication
  act app
  runEffectFn3 _listenPipe app pipe (mkEffectFn1 cb)

-- | Apply App actions to existent Express.js application
apply :: App -> Application -> Effect Unit
apply (AppM act) app = act app

-- | Use specified middleware handler.
use :: Handler -> App
use middleware = AppM \app ->
  runEffectFn2 _use app $ runHandlerM middleware

-- | Use any function as middleware.
-- | Introduced to ease usage of a bunch of external
-- | middleware written for express.js.
-- | See http://expressjs.com/4x/api.html#middleware
useExternal :: Middleware -> App
useExternal fn = AppM \app ->
  runEffectFn2 _use app fn

-- | Use specified middleware only on requests matching path.
useAt :: Path -> Handler -> App
useAt route middleware = AppM \app ->
  runEffectFn3 _useAt app route $ runHandlerM middleware

-- | Use any function as middleware only on requests matching path.
-- | Introduced to ease usage of a bunch of external
-- | middleware written for express.js.
-- | See http://expressjs.com/4x/api.html#middleware
useAtExternal :: Path -> Middleware -> App
useAtExternal route middleware = AppM \app ->
  runEffectFn3 _useAt app route middleware

-- | Process route param with specified handler.
param :: String -> (String -> Handler) -> App
param paramId mkHandler = AppM \app ->
  runEffectFn3 _param app paramId (runParamHandlerM mkHandler)

-- | Use error handler. Probably this should be the last middleware to attach.
useOnError :: (Error -> Handler) -> App
useOnError mkHandler = AppM \app ->
  runEffectFn2 _useOnError app (runErrorHandlerM mkHandler)

-- | Get application property.
-- | See http://expressjs.com/4x/api.html#app-settings
getProp :: forall a. String -> AppM (Maybe a)
getProp name = AppM \app ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getProp app name

-- | Set application property.
-- | See http://expressjs.com/4x/api.html#app-settings
setProp :: forall a. String -> a -> App
setProp name val = AppM \app ->
  runEffectFn3 _setProp app name val

-- | Bind specified handler to handle request matching route and method.
http :: forall r. (RoutePattern r) => Method -> r -> Handler -> App
http method route handler = AppM \app ->
  runEffectFn4 _http app (show method) (unsafeToForeign route) $ runHandlerM handler

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
foreign import _getProp :: forall a. EffectFn2 Application String (Nullable a)
foreign import _setProp :: forall a. EffectFn3 Application String a Unit
foreign import _http :: EffectFn4 Application String Foreign HandlerFnInternal_Req_Res_Next Unit
foreign import _httpServer :: EffectFn1 Application HttpServer
foreign import _httpsServer :: EffectFn1 Application HttpsServer
foreign import _listenHttp :: EffectFn3 Application Port (EffectFn1 Event Unit) HttpServer
foreign import _listenHttps :: forall opts. EffectFn4 Application Port opts (EffectFn1 Event Unit) HttpsServer
foreign import _listenHostHttp :: EffectFn4 Application Port Host (EffectFn1 Event Unit) HttpServer
foreign import _listenHostHttps :: forall opts. EffectFn5 Application Port Host opts (EffectFn1 Event Unit) HttpsServer
foreign import _listenPipe :: EffectFn3 Application Pipe (EffectFn1 Event Unit) HttpServer
foreign import _use :: EffectFn2 Application HandlerFnInternal_Req_Res_Next Unit
foreign import _useAt :: EffectFn3 Application Path HandlerFnInternal_Req_Res_Next Unit
foreign import _param :: EffectFn3 Application String HandlerFnInternal_Req_Res_Next_Param Unit

_useOnError :: EffectFn2 Application HandlerFnInternal_Err_Req_Res_Next Unit
_useOnError = unsafeCoerce _use
