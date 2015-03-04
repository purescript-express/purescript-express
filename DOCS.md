# Module Documentation

## Module Node.Express.App

#### `AppM`

``` purescript
data AppM a
```

Monad responsible for application related operations (initial setup mostly).

#### `App`

``` purescript
type App = AppM Unit
```


#### `functorAppM`

``` purescript
instance functorAppM :: Functor AppM
```


#### `applyAppM`

``` purescript
instance applyAppM :: Apply AppM
```


#### `applicativeAppM`

``` purescript
instance applicativeAppM :: Applicative AppM
```


#### `bindAppM`

``` purescript
instance bindAppM :: Bind AppM
```


#### `monadAppM`

``` purescript
instance monadAppM :: Monad AppM
```


#### `monadEffAppM`

``` purescript
instance monadEffAppM :: MonadEff eff AppM
```


#### `listenHttp`

``` purescript
listenHttp :: forall e. App -> Port -> (Event -> Eff e Unit) -> ExpressM Unit
```

Run application on specified port and execute callback after launch.
HTTP version

#### `listenHttps`

``` purescript
listenHttps :: forall e opts. App -> Port -> opts -> (Event -> Eff e Unit) -> ExpressM Unit
```

Run application on specified port and execute callback after launch.
HTTPS version

#### `apply`

``` purescript
apply :: App -> Application -> ExpressM Unit
```

Apply App actions to existent Express.js application

#### `use`

``` purescript
use :: Handler -> App
```

Use specified middleware handler.

#### `useExternal`

``` purescript
useExternal :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit) -> App
```

Use any function as middleware.
Introduced to ease usage of a bunch of external
middleware written for express.js.
See http://expressjs.com/4x/api.html#middleware

#### `useAt`

``` purescript
useAt :: Path -> Handler -> App
```

Use specified middleware only on requests matching path.

#### `useOnParam`

``` purescript
useOnParam :: String -> (String -> Handler) -> App
```

Process route param with specified handler.

#### `useOnError`

``` purescript
useOnError :: (Error -> Handler) -> App
```

Use error handler. Probably this should be the last middleware to attach.

#### `getProp`

``` purescript
getProp :: forall a. (IsForeign a) => String -> AppM (Maybe a)
```

Get application property.
See http://expressjs.com/4x/api.html#app-settings

#### `setProp`

``` purescript
setProp :: forall a. (IsForeign a) => String -> a -> App
```

Set application property.
See http://expressjs.com/4x/api.html#app-settings

#### `http`

``` purescript
http :: forall r. (RoutePattern r) => Method -> r -> Handler -> App
```

Bind specified handler to handle request matching route and method.

#### `get`

``` purescript
get :: forall r. (RoutePattern r) => r -> Handler -> App
```

Shortcut for `http GET`.

#### `post`

``` purescript
post :: forall r. (RoutePattern r) => r -> Handler -> App
```

Shortcut for `http POST`.

#### `put`

``` purescript
put :: forall r. (RoutePattern r) => r -> Handler -> App
```

Shortcut for `http PUT`.

#### `delete`

``` purescript
delete :: forall r. (RoutePattern r) => r -> Handler -> App
```

Shortcut for `http DELETE`.

#### `all`

``` purescript
all :: forall r. (RoutePattern r) => r -> Handler -> App
```

Shortcut for `http ALL` (match on any http method).


## Module Node.Express.Handler

#### `HandlerM`

``` purescript
data HandlerM a
```

Monad responsible for handling single request.

#### `Handler`

``` purescript
type Handler = HandlerM Unit
```


#### `functorHandlerM`

``` purescript
instance functorHandlerM :: Functor HandlerM
```


#### `applyHandlerM`

``` purescript
instance applyHandlerM :: Apply HandlerM
```


#### `applicativeHandlerM`

``` purescript
instance applicativeHandlerM :: Applicative HandlerM
```


#### `bindHandlerM`

``` purescript
instance bindHandlerM :: Bind HandlerM
```


#### `monadHandlerM`

``` purescript
instance monadHandlerM :: Monad HandlerM
```


#### `monadEffHandlerM`

``` purescript
instance monadEffHandlerM :: MonadEff eff HandlerM
```


#### `withHandler`

``` purescript
withHandler :: forall a. HandlerM a -> Request -> Response -> ExpressM Unit -> ExpressM a
```


#### `capture`

``` purescript
capture :: forall a b eff. (a -> HandlerM b) -> HandlerM (a -> Eff eff b)
```

Generate a closure from a function capturing current request and response.
It is intended to use with async functions like `fs.readFile`.
Example:

    fileReadHandler :: Handler
    fileReadHandler = do
        callback <- capture $ \data ->
            send data
        fs.readFile("some_file.txt", callback)


#### `next`

``` purescript
next :: Handler
```

Call next handler/middleware in a chain.

#### `nextThrow`

``` purescript
nextThrow :: Error -> Handler
```

Call next handler/middleware and pass error to it.

#### `getRouteParam`

``` purescript
getRouteParam :: forall a. (RequestParam a) => a -> HandlerM (Maybe String)
```

#### `getBodyParam`

``` purescript
getBodyParam :: forall a. (IsForeign a) => String -> HandlerM (Maybe a)
```

Get param from request's body.
NOTE: Not parsed by default, you must attach proper middleware
      See http://expressjs.com/4x/api.html#req.body

#### `getQueryParam`

``` purescript
getQueryParam :: String -> HandlerM (Maybe String)
```

Get param from query string (part of URL behind '?').
If there are multiple params having equal keys
return the first one.

#### `getQueryParams`

``` purescript
getQueryParams :: String -> HandlerM [String]
```

Get all params from query string having specified key.

#### `getRoute`

``` purescript
getRoute :: HandlerM String
```

Return route that matched this request.

#### `getCookie`

``` purescript
getCookie :: String -> HandlerM (Maybe String)
```

Get cookie param by its key.

#### `getSignedCookie`

``` purescript
getSignedCookie :: String -> HandlerM (Maybe String)
```

Get signed cookie param by its key.

#### `getRequestHeader`

``` purescript
getRequestHeader :: String -> HandlerM (Maybe String)
```

Get request header param.

#### `accepts`

``` purescript
accepts :: String -> HandlerM (Maybe String)
```

Check if specified response type will be accepted by a client.

#### `ifAccepts`

``` purescript
ifAccepts :: String -> Handler -> Handler
```

Execute specified handler if client accepts specified response type.

#### `acceptsCharset`

``` purescript
acceptsCharset :: String -> HandlerM (Maybe String)
```

Check if specified charset is accepted.

#### `acceptsLanguage`

``` purescript
acceptsLanguage :: String -> HandlerM (Maybe String)
```

Check if specified language is accepted.

#### `hasType`

``` purescript
hasType :: String -> HandlerM Boolean
```

Check if request's Content-Type field matches type.
See http://expressjs.com/4x/api.html#req.is

#### `getRemoteIp`

``` purescript
getRemoteIp :: HandlerM String
```

Return remote or upstream address.

#### `getRemoteIps`

``` purescript
getRemoteIps :: HandlerM [String]
```

Return list of X-Forwarded-For proxies if any.

#### `getPath`

``` purescript
getPath :: HandlerM String
```

Return request URL pathname.

#### `getHostname`

``` purescript
getHostname :: HandlerM String
```

Return Host header field.

#### `getSubdomains`

``` purescript
getSubdomains :: HandlerM [String]
```

Return array of subdomains.

#### `isFresh`

``` purescript
isFresh :: HandlerM Boolean
```

Check that Last-Modified and/or ETag still matches.

#### `isStale`

``` purescript
isStale :: HandlerM Boolean
```

Check that Last-Modified and/or ETag do not match.

#### `isXhr`

``` purescript
isXhr :: HandlerM Boolean
```

Check if request was issued by XMLHttpRequest.

#### `getProtocol`

``` purescript
getProtocol :: HandlerM (Maybe Protocol)
```

Return request protocol.

#### `getMethod`

``` purescript
getMethod :: HandlerM (Maybe Method)
```

Return request HTTP method

#### `getUrl`

``` purescript
getUrl :: HandlerM String
```

Return request URL (may be modified by other handlers/middleware).

#### `getOriginalUrl`

``` purescript
getOriginalUrl :: HandlerM String
```

Return request original URL.

#### `setStatus`

``` purescript
setStatus :: Number -> Handler
```

#### `getResponseHeader`

``` purescript
getResponseHeader :: forall a. (IsForeign a) => String -> HandlerM (Maybe a)
```

Return response header value.

#### `setResponseHeader`

``` purescript
setResponseHeader :: forall a. String -> a -> Handler
```

Set response header value.

#### `headersSent`

``` purescript
headersSent :: HandlerM Boolean
```

Check if headers have been sent already

#### `setCookie`

``` purescript
setCookie :: String -> String -> CookieOptions -> Handler
```

Set cookie by its name using specified options (maxAge, path, etc).

#### `clearCookie`

``` purescript
clearCookie :: String -> String -> Handler
```

Clear cookie.

#### `send`

``` purescript
send :: forall a. a -> Handler
```

Send a response. Could be object, string, buffer, etc.

#### `sendJson`

``` purescript
sendJson :: forall a. a -> Handler
```

Send a JSON response. Necessary headers are set automatically.

#### `sendJsonp`

``` purescript
sendJsonp :: forall a. a -> Handler
```

Send a JSON response with JSONP support.

#### `redirect`

``` purescript
redirect :: String -> Handler
```

Redirect to the given URL setting status to 302.

#### `setLocation`

``` purescript
setLocation :: String -> Handler
```

Set Location header.

#### `setContentType`

``` purescript
setContentType :: String -> Handler
```

Set Content-Type header.

#### `sendFile`

``` purescript
sendFile :: String -> Handler
```

Send file by its path.

#### `sendFileExt`

``` purescript
sendFileExt :: forall o. String -> {  | o } -> (Error -> ExpressM Unit) -> Handler
```

Send file by its path using specified options and error handler.
See http://expressjs.com/4x/api.html#res.sendfile

#### `download`

``` purescript
download :: String -> Handler
```

Transfer file as an attachment (will prompt user to download).

#### `downloadExt`

``` purescript
downloadExt :: String -> String -> (Error -> ExpressM Unit) -> Handler
```

Transfer file as an attachment using specified filename and error handler.


## Module Node.Express.Types

#### `Express`

``` purescript
data Express :: !
```


#### `ExpressM`

``` purescript
type ExpressM a = forall e. Eff (express :: Express | e) a
```

General monad, indicates that we're dealing with
express.js related functions.
Applications should use HandlerM and AppM primarily
and ExpressM in rare cases.

#### `Application`

``` purescript
data Application :: *
```


#### `Event`

``` purescript
data Event :: *
```


#### `Response`

``` purescript
data Response :: *
```


#### `Request`

``` purescript
data Request :: *
```


#### `Protocol`

``` purescript
data Protocol
  = Http 
  | Https 
```


#### `isForeignProtocol`

``` purescript
instance isForeignProtocol :: IsForeign Protocol
```


#### `Method`

``` purescript
data Method
  = ALL 
  | GET 
  | POST 
  | PUT 
  | DELETE 
  | OPTIONS 
  | HEAD 
  | TRACE 
  | CustomMethod String
```


#### `showMethod`

``` purescript
instance showMethod :: Show Method
```


#### `isForeignMethod`

``` purescript
instance isForeignMethod :: IsForeign Method
```


#### `Port`

``` purescript
type Port = Number
```


#### `Path`

``` purescript
type Path = String
```


#### `RoutePattern`

``` purescript
class RoutePattern a where
```


#### `routePath`

``` purescript
instance routePath :: RoutePattern String
```


#### `routeRegex`

``` purescript
instance routeRegex :: RoutePattern Regex
```


#### `RequestParam`

``` purescript
class RequestParam a where
```


#### `requestParamString`

``` purescript
instance requestParamString :: RequestParam String
```


#### `requestParamNumber`

``` purescript
instance requestParamNumber :: RequestParam Number
```


#### `CookieOptions`

``` purescript
newtype CookieOptions
  = CookieOptions { path :: String, signed :: Boolean, maxAge :: Number }
```

Cookie options
- maxAge -- time in msecs
- signed -- use secret to sign if true
- path   -- cookie path

#### `defaultCookieOptions`

``` purescript
instance defaultCookieOptions :: Default CookieOptions
```



## Module Data.Default

#### `Default`

``` purescript
class Default a where
  def :: a
```