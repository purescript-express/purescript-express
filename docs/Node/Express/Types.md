## Module Node.Express.Types

#### `EXPRESS`

``` purescript
data EXPRESS :: !
```

#### `ExpressM`

``` purescript
type ExpressM e a = Eff (express :: EXPRESS | e) a
```

General monad, indicates that we're dealing with
express.js related functions.
Applications should use HandlerM and AppM primarily
and ExpressM in rare cases.

#### `Application`

``` purescript
data Application :: *
```

#### `Event`

``` purescript
data Event :: *
```

#### `Response`

``` purescript
data Response :: *
```

#### `Request`

``` purescript
data Request :: *
```

#### `Protocol`

``` purescript
data Protocol
  = Http
  | Https
```

##### Instances
``` purescript
Show Protocol
IsForeign Protocol
```

#### `Method`

``` purescript
data Method
  = ALL
  | GET
  | POST
  | PUT
  | DELETE
  | OPTIONS
  | HEAD
  | TRACE
  | CustomMethod String
```

##### Instances
``` purescript
Show Method
IsForeign Method
```

#### `Port`

``` purescript
type Port = Int
```

#### `Path`

``` purescript
type Path = String
```

#### `RoutePattern`

``` purescript
class RoutePattern a 
```

##### Instances
``` purescript
RoutePattern String
RoutePattern Regex
```

#### `RequestParam`

``` purescript
class RequestParam a 
```

##### Instances
``` purescript
RequestParam String
RequestParam Number
```

#### `CookieOptions`

``` purescript
newtype CookieOptions
  = CookieOptions { maxAge :: Int, signed :: Boolean, path :: String }
```

Cookie options
- maxAge -- time in msecs
- signed -- use secret to sign if true
- path   -- cookie path

##### Instances
``` purescript
Default CookieOptions
```


