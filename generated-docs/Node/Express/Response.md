## Module Node.Express.Response

#### `setStatus`

``` purescript
setStatus :: Int -> Handler
```

Set status code.

#### `getResponseHeader`

``` purescript
getResponseHeader :: String -> HandlerM (Maybe String)
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

#### `setContentType`

``` purescript
setContentType :: String -> Handler
```

Set Content-Type header.

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

#### `redirectWithStatus`

``` purescript
redirectWithStatus :: Int -> String -> Handler
```

Redirect to the given URL using custom status.

#### `setLocation`

``` purescript
setLocation :: String -> Handler
```

Set Location header.

#### `sendFile`

``` purescript
sendFile :: String -> Handler
```

Send file by its path.

#### `sendFileExt`

``` purescript
sendFileExt :: forall o. String -> {  | o } -> (Error -> Effect Unit) -> Handler
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
downloadExt :: String -> String -> (Error -> Effect Unit) -> Handler
```

Transfer file as an attachment using specified filename and error handler.

#### `render`

``` purescript
render :: forall a. String -> a -> Handler
```

Render a view with a view model object. Could be object, string, buffer, etc.

#### `end`

``` purescript
end :: Handler
```

Ends the response process.


