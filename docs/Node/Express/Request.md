## Module Node.Express.Request

#### `getRouteParam`

``` purescript
getRouteParam :: forall e a. RequestParam a => a -> HandlerM (express :: EXPRESS | e) (Maybe String)
```

Get route param value. If it is named route, e.g `/user/:id` then
`getRouteParam "id"` return matched part of route. If it is
regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
part that matched `(\d+)` and `getRouteParam 0` return whole
route.

#### `getBody`

``` purescript
getBody :: forall e a. IsForeign a => HandlerM (express :: EXPRESS | e) (Either MultipleErrors a)
```

Get the request's body.
NOTE: Not parsed by default, you must attach proper middleware
      See http://expressjs.com/4x/api.html#req.body

#### `getBodyParam`

``` purescript
getBodyParam :: forall e a. IsForeign a => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
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

#### `setUserData`

``` purescript
setUserData :: forall a e. String -> a -> Handler e
```

Sets the specified field of the userData object attached to the Request 
object to specified data

#### `getUserData`

``` purescript
getUserData :: forall e a. IsForeign a => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
```

Retrieves the data from the request set with previous call to `setUserData`


