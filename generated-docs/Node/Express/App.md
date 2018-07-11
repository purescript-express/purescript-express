## Module Node.Express.App

#### `AppM`

``` purescript
data AppM a
```

Monad responsible for application related operations (initial setup mostly).

##### Instances
``` purescript
Functor AppM
Apply AppM
Applicative AppM
Bind AppM
Monad AppM
MonadEffect AppM
```

#### `App`

``` purescript
type App = AppM Unit
```

#### `listenHttp`

``` purescript
listenHttp :: App -> Port -> (Event -> Effect Unit) -> Effect Server
```

Run application on specified port and execute callback after launch.
HTTP version

#### `listenHttps`

``` purescript
listenHttps :: forall opts. App -> Port -> opts -> (Event -> Effect Unit) -> Effect Server
```

Run application on specified port and execute callback after launch.
HTTPS version

#### `listenHostHttp`

``` purescript
listenHostHttp :: App -> Port -> Host -> (Event -> Effect Unit) -> Effect Server
```

Run application on specified port & host and execute callback after launch.
HTTP version

#### `listenHostHttps`

``` purescript
listenHostHttps :: forall opts. App -> Port -> Host -> opts -> (Event -> Effect Unit) -> Effect Server
```

Run application on specified port & host and execute callback after launch.
HTTPS version

#### `listenPipe`

``` purescript
listenPipe :: App -> Pipe -> (Event -> Effect Unit) -> Effect Server
```

Run application on specified named pipe and execute callback after launch.
HTTP version

#### `makeHttpServer`

``` purescript
makeHttpServer :: App -> Effect Server
```

Create a Node.HTTP server from the Express application.
HTTP version

#### `makeHttpsServer`

``` purescript
makeHttpsServer :: App -> Effect Server
```

Create a Node.HTTP server from the Express application.
HTTPS version

#### `apply`

``` purescript
apply :: App -> Application -> Effect Unit
```

Apply App actions to existent Express.js application

#### `use`

``` purescript
use :: Handler -> App
```

Use specified middleware handler.

#### `useExternal`

``` purescript
useExternal :: Fn3 Request Response (Effect Unit) (Effect Unit) -> App
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

#### `useAtExternal`

``` purescript
useAtExternal :: Path -> Fn3 Request Response (Effect Unit) (Effect Unit) -> App
```

Use any function as middleware only on requests matching path.
Introduced to ease usage of a bunch of external
middleware written for express.js.
See http://expressjs.com/4x/api.html#middleware

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
getProp :: forall a. String -> AppM (Maybe a)
```

Get application property.
See http://expressjs.com/4x/api.html#app-settings

#### `setProp`

``` purescript
setProp :: forall a. String -> a -> App
```

Set application property.
See http://expressjs.com/4x/api.html#app-settings

#### `http`

``` purescript
http :: forall r. RoutePattern r => Method -> r -> Handler -> App
```

Bind specified handler to handle request matching route and method.

#### `get`

``` purescript
get :: forall r. RoutePattern r => r -> Handler -> App
```

Shortcut for `http GET`.

#### `post`

``` purescript
post :: forall r. RoutePattern r => r -> Handler -> App
```

Shortcut for `http POST`.

#### `put`

``` purescript
put :: forall r. RoutePattern r => r -> Handler -> App
```

Shortcut for `http PUT`.

#### `delete`

``` purescript
delete :: forall r. RoutePattern r => r -> Handler -> App
```

Shortcut for `http DELETE`.

#### `all`

``` purescript
all :: forall r. RoutePattern r => r -> Handler -> App
```

Shortcut for `http ALL` (match on any http method).


