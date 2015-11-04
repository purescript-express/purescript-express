## Module Node.Express.Handler

#### `HandlerM`

``` purescript
data HandlerM e a
```

Monad responsible for handling single request.

##### Instances
``` purescript
instance functorHandlerM :: Functor (HandlerM e)
instance applyHandlerM :: Apply (HandlerM e)
instance applicativeHandlerM :: Applicative (HandlerM e)
instance bindHandlerM :: Bind (HandlerM e)
instance monadHandlerM :: Monad (HandlerM e)
instance monadEffHandlerM :: MonadEff eff (HandlerM eff)
instance monadAffHandlerM :: MonadAff eff (HandlerM eff)
```

#### `Handler`

``` purescript
type Handler e = HandlerM (express :: EXPRESS | e) Unit
```

#### `runHandlerM`

``` purescript
runHandlerM :: forall e. Handler e -> Request -> Response -> ExpressM e Unit -> ExpressM e Unit
```

#### `next`

``` purescript
next :: forall e. Handler e
```

Call next handler/middleware in a chain.

#### `nextThrow`

``` purescript
nextThrow :: forall e a. Error -> HandlerM (express :: EXPRESS | e) a
```

Call next handler/middleware and pass error to it.

#### `getRouteParam`

``` purescript
getRouteParam :: forall e a. (RequestParam a) => a -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

#### `getBodyParam`

``` purescript
getBodyParam :: forall e a. (IsForeign a) => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
```

Get param from request's body.
NOTE: Not parsed by default, you must attach proper middleware
      See http://expressjs.com/4x/api.html#req.body

#### `getQueryParam`

``` purescript
getQueryParam :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Get param from query string (part of URL behind '?').
If there are multiple params having equal keys
return the first one.

#### `getQueryParams`

``` purescript
getQueryParams :: forall e. String -> HandlerM (express :: EXPRESS | e) (Array String)
```

Get all params from query string having specified key.

#### `getRoute`

``` purescript
getRoute :: forall e. HandlerM (express :: EXPRESS | e) String
```

Return route that matched this request.

#### `getCookie`

``` purescript
getCookie :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Get cookie param by its key.

#### `getSignedCookie`

``` purescript
getSignedCookie :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Get signed cookie param by its key.

#### `getRequestHeader`

``` purescript
getRequestHeader :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Get request header param.

#### `accepts`

``` purescript
accepts :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Check if specified response type will be accepted by a client.

#### `ifAccepts`

``` purescript
ifAccepts :: forall e. String -> Handler e -> Handler e
```

Execute specified handler if client accepts specified response type.

#### `acceptsCharset`

``` purescript
acceptsCharset :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Check if specified charset is accepted.

#### `acceptsLanguage`

``` purescript
acceptsLanguage :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Check if specified language is accepted.

#### `hasType`

``` purescript
hasType :: forall e. String -> HandlerM (express :: EXPRESS | e) Boolean
```

Check if request's Content-Type field matches type.
See http://expressjs.com/4x/api.html#req.is

#### `getRemoteIp`

``` purescript
getRemoteIp :: forall e. HandlerM (express :: EXPRESS | e) String
```

Return remote or upstream address.

#### `getRemoteIps`

``` purescript
getRemoteIps :: forall e. HandlerM (express :: EXPRESS | e) (Array String)
```

Return list of X-Forwarded-For proxies if any.

#### `getPath`

``` purescript
getPath :: forall e. HandlerM (express :: EXPRESS | e) String
```

Return request URL pathname.

#### `getHostname`

``` purescript
getHostname :: forall e. HandlerM (express :: EXPRESS | e) String
```

Return Host header field.

#### `getSubdomains`

``` purescript
getSubdomains :: forall e. HandlerM (express :: EXPRESS | e) (Array String)
```

Return array of subdomains.

#### `isFresh`

``` purescript
isFresh :: forall e. HandlerM (express :: EXPRESS | e) Boolean
```

Check that Last-Modified and/or ETag still matches.

#### `isStale`

``` purescript
isStale :: forall e. HandlerM (express :: EXPRESS | e) Boolean
```

Check that Last-Modified and/or ETag do not match.

#### `isXhr`

``` purescript
isXhr :: forall e. HandlerM (express :: EXPRESS | e) Boolean
```

Check if request was issued by XMLHttpRequest.

#### `getProtocol`

``` purescript
getProtocol :: forall e. HandlerM (express :: EXPRESS | e) (Maybe Protocol)
```

Return request protocol.

#### `getMethod`

``` purescript
getMethod :: forall e. HandlerM (express :: EXPRESS | e) (Maybe Method)
```

Return request HTTP method

#### `getUrl`

``` purescript
getUrl :: forall e. HandlerM (express :: EXPRESS | e) String
```

Return request URL (may be modified by other handlers/middleware).

#### `getOriginalUrl`

``` purescript
getOriginalUrl :: forall e. HandlerM (express :: EXPRESS | e) String
```

Return request original URL.

#### `setStatus`

``` purescript
setStatus :: forall e. Int -> Handler e
```

#### `getResponseHeader`

``` purescript
getResponseHeader :: forall e a. (IsForeign a) => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
```

Return response header value.

#### `setResponseHeader`

``` purescript
setResponseHeader :: forall e a. String -> a -> Handler e
```

Set response header value.

#### `headersSent`

``` purescript
headersSent :: forall e. HandlerM (express :: EXPRESS | e) Boolean
```

Check if headers have been sent already

#### `setCookie`

``` purescript
setCookie :: forall e. String -> String -> CookieOptions -> Handler e
```

Set cookie by its name using specified options (maxAge, path, etc).

#### `clearCookie`

``` purescript
clearCookie :: forall e. String -> String -> Handler e
```

Clear cookie.

#### `send`

``` purescript
send :: forall e a. a -> Handler e
```

Send a response. Could be object, string, buffer, etc.

#### `sendJson`

``` purescript
sendJson :: forall e a. a -> Handler e
```

Send a JSON response. Necessary headers are set automatically.

#### `sendJsonp`

``` purescript
sendJsonp :: forall e a. a -> Handler e
```

Send a JSON response with JSONP support.

#### `redirect`

``` purescript
redirect :: forall e. String -> Handler e
```

Redirect to the given URL setting status to 302.

#### `redirectWithStatus`

``` purescript
redirectWithStatus :: forall e. Int -> String -> Handler e
```

Redirect to the given URL using custom status.

#### `setLocation`

``` purescript
setLocation :: forall e. String -> Handler e
```

Set Location header.

#### `setContentType`

``` purescript
setContentType :: forall e. String -> Handler e
```

Set Content-Type header.

#### `sendFile`

``` purescript
sendFile :: forall e. String -> Handler e
```

Send file by its path.

#### `sendFileExt`

``` purescript
sendFileExt :: forall e o. String -> {  | o } -> (Error -> ExpressM e Unit) -> Handler e
```

Send file by its path using specified options and error handler.
See http://expressjs.com/4x/api.html#res.sendfile

#### `download`

``` purescript
download :: forall e. String -> Handler e
```

Transfer file as an attachment (will prompt user to download).

#### `downloadExt`

``` purescript
downloadExt :: forall e. String -> String -> (Error -> ExpressM e Unit) -> Handler e
```

Transfer file as an attachment using specified filename and error handler.


