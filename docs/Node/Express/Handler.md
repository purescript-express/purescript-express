## Module Node.Express.Handler

#### `HandlerM`

``` purescript
data HandlerM a
```

Monad responsible for handling single request.

##### Instances
``` purescript
instance functorHandlerM :: Functor HandlerM
instance applyHandlerM :: Apply HandlerM
instance applicativeHandlerM :: Applicative HandlerM
instance bindHandlerM :: Bind HandlerM
instance monadHandlerM :: Monad HandlerM
instance monadEffHandlerM :: MonadEff eff HandlerM
```

#### `Handler`

``` purescript
type Handler = HandlerM Unit
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
nextThrow :: forall a. Error -> HandlerM a
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
getQueryParams :: String -> HandlerM (Array String)
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
getRemoteIps :: HandlerM (Array String)
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
getSubdomains :: HandlerM (Array String)
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
setStatus :: Int -> Handler
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


