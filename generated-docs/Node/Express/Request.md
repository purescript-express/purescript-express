## Module Node.Express.Request

#### `getRouteParam`

``` purescript
getRouteParam :: forall a. RequestParam a => a -> HandlerM (Maybe String)
```

Get route param value. If it is named route, e.g `/user/:id` then
`getRouteParam "id"` return matched part of route. If it is
regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
part that matched `(\d+)` and `getRouteParam 0` return whole
route.

#### `getQueryParam`

``` purescript
getQueryParam :: forall a. String -> HandlerM (Maybe a)
```

Get param from query string (part of URL behind '?').
It could be any JS object, e.g. an array in case multiple repeating query
parameters, or an object in case of nested query parameters (this
particular behavior depends on the type of query parser - 'simple' won't
parse complex objects, see
https://github.com/expressjs/express/blob/master/test/req.query.js)

#### `getQueryParams`

``` purescript
getQueryParams :: forall a. String -> HandlerM (Maybe (Array a))
```

Shortcut for `getQueryParam paramName :: HandlerM (Maybe (Array a))`

#### `getBody`

``` purescript
getBody :: forall a. Decode a => HandlerM (F a)
```

Get the request's body.
NOTE: Not parsed by default, you must attach proper middleware
      See http://expressjs.com/4x/api.html#req.body

#### `getBody'`

``` purescript
getBody' :: HandlerM Foreign
```

Get the request's body without a `Decode` parsing.
NOTE: Not parsed by default, you must attach proper middleware
      See http://expressjs.com/4x/api.html#req.body

#### `getBodyParam`

``` purescript
getBodyParam :: forall a. String -> HandlerM (Maybe a)
```

Get param from request's body.
NOTE: Not parsed by default, you must attach proper middleware
      See http://expressjs.com/4x/api.html#req.body

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
getMethod :: HandlerM Method
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

#### `getUserData`

``` purescript
getUserData :: forall a. String -> HandlerM (Maybe a)
```

Retrieves the data from the request set with previous call to `setUserData`

#### `setUserData`

``` purescript
setUserData :: forall a. String -> a -> Handler
```

Sets the specified field of the userData object attached to the Request
object to specified data


