## Module Node.Express.Middleware.CookieParser

#### `_cookieParser`

``` purescript
_cookieParser :: forall eff. Fn3 Request Response (ExpressM eff Unit) (ExpressM eff Unit)
```

#### `cookieParser`

``` purescript
cookieParser :: forall eff. Handler eff
```

Handler that parses cookies using 'cookie-parser' middleware.


