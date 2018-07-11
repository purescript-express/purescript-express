## Module Node.Express.Types

#### `Application`

``` purescript
data Application :: Type
```

#### `Event`

``` purescript
data Event :: Type
```

#### `Response`

``` purescript
data Response :: Type
```

#### `Request`

``` purescript
data Request :: Type
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
```

#### `decodeProtocol`

``` purescript
decodeProtocol :: String -> Maybe Protocol
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
```

#### `decodeMethod`

``` purescript
decodeMethod :: String -> Method
```

#### `Host`

``` purescript
type Host = String
```

#### `Port`

``` purescript
type Port = Int
```

#### `Pipe`

``` purescript
type Pipe = String
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


