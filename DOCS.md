# Module Documentation
## Node.Express.App
### Types
```haskell
data AppM a
```
 Monad responsible for application related operations (initial setup mostly).

```haskell
type App = AppM Unit
```
### Instances
```haskell
instance functorAppM :: Functor AppM
```
```haskell
instance applyAppM :: Apply AppM
```
```haskell
instance applicativeAppM :: Applicative AppM
```
```haskell
instance bindAppM :: Bind AppM
```
```haskell
instance monadAppM :: Monad AppM
```
```haskell
instance monadEffAppM :: MonadEff AppM
```
### Values
```haskell
listen :: forall e. App -> Port -> (Event -> Eff e Unit) -> ExpressM Unit
```
 Run application on specified port and execute callback after launch.

```haskell
use :: Handler -> App
```
 Use specified middleware handler.

```haskell
useExternal :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit) -> App
```
 Use any function as middleware.
  Introduced to ease usage of a bunch of external
  middleware written for express.js.
  See http://expressjs.com/4x/api.html#middleware

```haskell
useAt :: Path -> Handler -> App
```
 Use specified middleware only on requests matching path.

```haskell
useOnParam :: String -> (String -> Handler) -> App
```
 Process route param with specified handler.

```haskell
useOnError :: (Error -> Handler) -> App
```
 Use error handler. Probably this should be the last middleware to attach.

```haskell
getProp :: forall a. (ReadForeign a) => String -> AppM (Maybe a)
```
 Get application property.
 See http://expressjs.com/4x/api.html#app-settings

```haskell
setProp :: forall a. (ReadForeign a) => String -> a -> App
```
 Set application property.
 See http://expressjs.com/4x/api.html#app-settings

```haskell
http :: forall r. (RoutePattern r) => Method -> r -> Handler -> App
```
 Bind specified handler to handle request matching route and method.

```haskell
get :: forall r. (RoutePattern r) => r -> Handler -> App
```
 Shortcut for `http GET`.

```haskell
post :: forall r. (RoutePattern r) => r -> Handler -> App
```
 Shortcut for `http POST`.

```haskell
put :: forall r. (RoutePattern r) => r -> Handler -> App
```
 Shortcut for `http PUT`.

```haskell
delete :: forall r. (RoutePattern r) => r -> Handler -> App
```
 Shortcut for `http DELETE`.

```haskell
all :: forall r. (RoutePattern r) => r -> Handler -> App
```
 Shortcut for `http ALL` (match on any http method).


## Node.Express.Handler
### Types
```haskell
data HandlerM a
```
 Monad responsible for handling single request.

```haskell
type Handler = HandlerM Unit
```
### Instances
```haskell
instance functorHandlerM :: Functor HandlerM
```
```haskell
instance applyHandlerM :: Apply HandlerM
```
```haskell
instance applicativeHandlerM :: Applicative HandlerM
```
```haskell
instance bindHandlerM :: Bind HandlerM
```
```haskell
instance monadHandlerM :: Monad HandlerM
```
```haskell
instance monadEffHandlerM :: MonadEff HandlerM
```
### Values
```haskell
withHandler :: Handler -> Request -> Response -> ExpressM Unit -> ExpressM Unit
```
```haskell
next :: Handler
```
 Call next handler/middleware in a chain.

```haskell
nextThrow :: Error -> Handler
```
 Call next handler/middleware and pass error to it.

```haskell
getRouteParam :: forall a. (RequestParam a) => a -> HandlerM (Maybe String)
```
 Get route param value. If it is named route, e.g `/user/:id` then
  `getRouteParam "id"` return matched part of route. If it is
  regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
  part that matched `(\d+)` and `getRouteParam 0` return whole
  route.

```haskell
getParam :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
```
 Get param regardless its origin.

```haskell
getQueryParam :: String -> HandlerM (Maybe String)
```
 Get param from query string (part of URL behind '?').
  If there are multiple params having equal keys
  return the first one.

```haskell
getQueryParams :: String -> HandlerM [String]
```
 Get all params from query string having specified key.

```haskell
getRoute :: HandlerM String
```
 Return route that matched this request.

```haskell
getCookie :: String -> HandlerM (Maybe String)
```
 Get cookie param by its key.

```haskell
getSignedCookie :: String -> HandlerM (Maybe String)
```
 Get signed cookie param by its key.

```haskell
getRequestHeader :: String -> HandlerM (Maybe String)
```
 Get request header param.

```haskell
accepts :: String -> HandlerM (Maybe String)
```
 Check if specified response type will be accepted by a client.

```haskell
ifAccepts :: String -> Handler -> Handler
```
 Execute specified handler if client accepts specified response type.

```haskell
acceptsCharset :: String -> HandlerM (Maybe String)
```
 Check if specified charset is accepted.

```haskell
acceptsLanguage :: String -> HandlerM (Maybe String)
```
 Check if specified language is accepted.

```haskell
hasType :: String -> HandlerM Boolean
```
 Check if request's Content-Type field matches type.
  See http://expressjs.com/4x/api.html#req.is

```haskell
getRemoteIp :: HandlerM String
```
 Return remote or upstream address.

```haskell
getRemoteIps :: HandlerM [String]
```
 Return list of X-Forwarded-For proxies if any.

```haskell
getPath :: HandlerM String
```
 Return request URL pathname.

```haskell
getHostname :: HandlerM String
```
 Return Host header field.

```haskell
getSubdomains :: HandlerM [String]
```
 Return array of subdomains.

```haskell
isFresh :: HandlerM Boolean
```
 Check that Last-Modified and/or ETag still matches.

```haskell
isStale :: HandlerM Boolean
```
 Check that Last-Modified and/or ETag do not match.

```haskell
isXhr :: HandlerM Boolean
```
 Check if request was issued by XMLHttpRequest.

```haskell
getProtocol :: HandlerM (Maybe Protocol)
```
 Return request protocol.

```haskell
getUrl :: HandlerM String
```
 Return request URL (may be modified by other handlers/middleware).

```haskell
getOriginalUrl :: HandlerM String
```
 Return request original URL.

```haskell
setStatus :: Number -> Handler
```
 Set status code.

```haskell
getResponseHeader :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
```
 Return response header value.

```haskell
setResponseHeader :: forall a. String -> a -> Handler
```
 Set response header value.

```haskell
setCookie :: String -> String -> CookieOptions -> Handler
```
 Set cookie by its name using specified options (maxAge, path, etc).

```haskell
clearCookie :: String -> String -> Handler
```
 Clear cookie.

```haskell
send :: forall a. a -> Handler
```
 Send a response. Could be object, string, buffer, etc.

```haskell
sendJson :: forall a. a -> Handler
```
 Send a JSON response. Necessary headers are set automatically.

```haskell
sendJsonp :: forall a. a -> Handler
```
 Send a JSON response with JSONP support.

```haskell
redirect :: String -> Handler
```
 Redirect to the given URL setting status to 302.

```haskell
setLocation :: String -> Handler
```
 Set Location header.

```haskell
setContentType :: String -> Handler
```
 Set Content-Type header.

```haskell
sendFile :: String -> Handler
```
 Send file by its path.

```haskell
sendFileExt :: forall o. String -> {  | o } -> (Error -> ExpressM Unit) -> Handler
```
 Send file by its path using specified options and error handler.
  See http://expressjs.com/4x/api.html#res.sendfile

```haskell
download :: String -> Handler
```
 Transfer file as an attachment (will prompt user to download).

```haskell
downloadExt :: String -> String -> (Error -> ExpressM Unit) -> Handler
```
 Transfer file as an attachment using specified filename and error handler.


## Node.Express.Types
### Types
```haskell
foreign data Express :: !
```
```haskell
type ExpressM a = forall e. Eff (express :: Express | e) a
```
 General monad, indicates that we're dealing with
  express.js related functions.
  Applications should use HandlerM and AppM primarily
  and ExpressM in rare cases.

```haskell
foreign data Application :: *
```
```haskell
foreign data Event :: *
```
```haskell
foreign data Error :: *
```
```haskell
foreign data Response :: *
```
```haskell
foreign data Request :: *
```
```haskell
data Protocol
	 Http :: Protocol
	 Https :: Protocol
```
```haskell
data Method
	 ALL :: Method
	 GET :: Method
	 POST :: Method
	 PUT :: Method
	 DELETE :: Method
```
```haskell
type Port = Number
```
```haskell
type Path = String
```
```haskell
data CookieOptions
	 CookieOptions :: { path :: String, signed :: Boolean, maxAge :: Number } -> CookieOptions
```
 Cookie options
  - maxAge -- time in msecs
  - signed -- use secret to sign if true
  - path   -- cookie path

### Typeclasses
```haskell
class RoutePattern a where
```
```haskell
class RequestParam a where
```
### Instances
```haskell
instance monadEffExpressM :: MonadEff (Eff e)
```
```haskell
instance readForeignProtocol :: ReadForeign Protocol
```
```haskell
instance showMethod :: Show Method
```
```haskell
instance routePath :: RoutePattern String
```
```haskell
instance routeRegex :: RoutePattern Regex
```
```haskell
instance requestParamString :: RequestParam String
```
```haskell
instance requestParamNumber :: RequestParam Number
```
```haskell
instance defaultCookieOptions :: Default CookieOptions
```
### Values
```haskell
error :: String -> Error
```
 Create new error and set its message.

```haskell
getErrorMsg :: Error -> String
```
 Extract message from error


## Control.Monad.Eff.Class
### Typeclasses
```haskell
class MonadEff m where
	 liftEff :: forall e a. Eff e a -> m a
```

## Data.Default
### Typeclasses
```haskell
class Default a where
	 def :: a
```

