## Module Node.Express.Handler

#### `HandlerM`

``` purescript
newtype HandlerM e a
  = HandlerM (Request -> Response -> Eff e Unit -> Aff e a)
```

Monad responsible for handling single request.

##### Instances
``` purescript
Functor (HandlerM e)
Apply (HandlerM e)
Applicative (HandlerM e)
Bind (HandlerM e)
Monad (HandlerM e)
MonadEff eff (HandlerM eff)
MonadAff eff (HandlerM eff)
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


