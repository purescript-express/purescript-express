## Module Node.Express.Internal.QueryString

#### `Param`

``` purescript
data Param
  = Param String String
```

##### Instances
``` purescript
Show Param
Eq Param
```

#### `parse`

``` purescript
parse :: String -> Either String (Array Param)
```

#### `getOne`

``` purescript
getOne :: (Array Param) -> String -> Maybe String
```

#### `getAll`

``` purescript
getAll :: (Array Param) -> String -> (Array String)
```


