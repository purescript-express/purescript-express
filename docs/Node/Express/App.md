## Module Node.Express.App

#### `AppM`

``` purescript
data AppM e a
```

Monad responsible for application related operations (initial setup mostly).

##### Instances
``` purescript
Functor (AppM e)
Apply (AppM e)
Applicative (AppM e)
Bind (AppM e)
Monad (AppM e)
MonadEff eff (AppM eff)
```

#### `App`

``` purescript
type App e = AppM (express :: EXPRESS | e) Unit
```

#### `listenHttp`

``` purescript
listenHttp :: forall e1 e2. App e1 -> Port -> (Event -> Eff e2 Unit) -> ExpressM e1 Server
```

Run application on specified port and execute callback after launch.
HTTP version

#### `listenHttps`

``` purescript
listenHttps :: forall e1 e2 opts. App e1 -> Port -> opts -> (Event -> Eff e2 Unit) -> ExpressM e1 Server
```

Run application on specified port and execute callback after launch.
HTTPS version

#### `apply`

``` purescript
apply :: forall e. App e -> Application -> ExpressM e Unit
```

Apply App actions to existent Express.js application

#### `use`

``` purescript
use :: forall e. Handler e -> App e
```

Use specified middleware handler.

#### `useExternal`

``` purescript
useExternal :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit) -> App e
```

Use any function as middleware.
Introduced to ease usage of a bunch of external
middleware written for express.js.
See http://expressjs.com/4x/api.html#middleware

#### `useAt`

``` purescript
useAt :: forall e. Path -> Handler e -> App e
```

Use specified middleware only on requests matching path.

#### `useOnParam`

``` purescript
useOnParam :: forall e. String -> (String -> Handler e) -> App e
```

Process route param with specified handler.

#### `useOnError`

``` purescript
useOnError :: forall e. (Error -> Handler e) -> App e
```

Use error handler. Probably this should be the last middleware to attach.

#### `getProp`

``` purescript
getProp :: forall e a. Decode a => String -> AppM (express :: EXPRESS | e) (Maybe a)
```

Get application property.
See http://expressjs.com/4x/api.html#app-settings

#### `setProp`

``` purescript
setProp :: forall e a. Decode a => String -> a -> App e
```

Set application property.
See http://expressjs.com/4x/api.html#app-settings

#### `http`

``` purescript
http :: forall e r. RoutePattern r => Method -> r -> Handler e -> App e
```

Bind specified handler to handle request matching route and method.

#### `get`

``` purescript
get :: forall e r. RoutePattern r => r -> Handler e -> App e
```

Shortcut for `http GET`.

#### `post`

``` purescript
post :: forall e r. RoutePattern r => r -> Handler e -> App e
```

Shortcut for `http POST`.

#### `put`

``` purescript
put :: forall e r. RoutePattern r => r -> Handler e -> App e
```

Shortcut for `http PUT`.

#### `delete`

``` purescript
delete :: forall e r. RoutePattern r => r -> Handler e -> App e
```

Shortcut for `http DELETE`.

#### `all`

``` purescript
all :: forall e r. RoutePattern r => r -> Handler e -> App e
```

Shortcut for `http ALL` (match on any http method).


