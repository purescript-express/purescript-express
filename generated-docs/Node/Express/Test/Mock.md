## Module Node.Express.Test.Mock

#### `MockResponse`

``` purescript
type MockResponse = { statusCode :: Int, headers :: Object String, "data" :: String, cookies :: Object MockCookie }
```

#### `MockRequest`

``` purescript
newtype MockRequest
  = MockRequest { setHeader :: String -> String -> MockRequest, setBody :: Foreign -> MockRequest, setBodyParam :: String -> String -> MockRequest, setRouteParam :: String -> String -> MockRequest, setCookie :: String -> String -> MockRequest, setSignedCookie :: String -> String -> MockRequest }
```

#### `MockCookie`

``` purescript
type MockCookie = { name :: String, value :: String, options :: String }
```

#### `setRequestHeader`

``` purescript
setRequestHeader :: String -> String -> MockRequest -> MockRequest
```

#### `setRouteParam`

``` purescript
setRouteParam :: String -> String -> MockRequest -> MockRequest
```

#### `setBody`

``` purescript
setBody :: String -> MockRequest -> MockRequest
```

#### `setBody'`

``` purescript
setBody' :: Foreign -> MockRequest -> MockRequest
```

#### `setBodyParam`

``` purescript
setBodyParam :: String -> String -> MockRequest -> MockRequest
```

#### `setRequestCookie`

``` purescript
setRequestCookie :: String -> String -> MockRequest -> MockRequest
```

#### `setRequestSignedCookie`

``` purescript
setRequestSignedCookie :: String -> String -> MockRequest -> MockRequest
```

#### `TestExpress`

``` purescript
type TestExpress = Aff
```

#### `TestMockApp`

``` purescript
type TestMockApp = ReaderT Application TestExpress Unit
```

#### `createMockApp`

``` purescript
createMockApp :: Effect Application
```

#### `createMockRequest`

``` purescript
createMockRequest :: String -> String -> Effect MockRequest
```

#### `testExpress`

``` purescript
testExpress :: String -> TestMockApp -> TestSuite
```

#### `setupMockApp`

``` purescript
setupMockApp :: TestApp -> TestMockApp
```

#### `sendRequest`

``` purescript
sendRequest :: Method -> String -> (MockRequest -> MockRequest) -> (MockResponse -> TestMockApp) -> TestMockApp
```

#### `sendError`

``` purescript
sendError :: Method -> String -> String -> (MockResponse -> TestMockApp) -> TestMockApp
```

#### `assertMatch`

``` purescript
assertMatch :: forall a. Show a => Eq a => String -> a -> a -> Test
```

#### `assertInApp`

``` purescript
assertInApp :: (Reporter -> TestApp) -> TestMockApp
```

#### `assertStatusCode`

``` purescript
assertStatusCode :: Int -> MockResponse -> TestMockApp
```

#### `assertHeader`

``` purescript
assertHeader :: String -> Maybe String -> MockResponse -> TestMockApp
```

#### `assertData`

``` purescript
assertData :: String -> MockResponse -> TestMockApp
```

#### `assertCookieValue`

``` purescript
assertCookieValue :: String -> Maybe String -> MockResponse -> TestMockApp
```

#### `setTestHeader`

``` purescript
setTestHeader :: String -> Handler
```

#### `assertTestHeader`

``` purescript
assertTestHeader :: Maybe String -> MockResponse -> TestMockApp
```


