## Module Node.Express.Response

#### `setStatus`

``` purescript
setStatus :: forall e. Int -> Handler e
```

Set status code.

#### `getResponseHeader`

``` purescript
getResponseHeader :: forall e a. Decode a => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
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

#### `setContentType`

``` purescript
setContentType :: forall e. String -> Handler e
```

Set Content-Type header.

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

#### `render`

``` purescript
render :: forall e a. String -> a -> Handler e
```

Render a view with a view model object. Could be object, string, buffer, etc.

#### `end`

``` purescript
end :: forall e. Handler e
```

Ends the response process.


