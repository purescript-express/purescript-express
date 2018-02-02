## Module Node.Express.Internal.Utils

#### `eitherToMaybe`

``` purescript
eitherToMaybe :: forall a e. Either e a -> Maybe a
```

#### `nextWithError`

``` purescript
nextWithError :: forall e a. Fn2 (ExpressM e Unit) Error (ExpressM e a)
```


