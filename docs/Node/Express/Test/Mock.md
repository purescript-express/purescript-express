## Module Node.Express.Test.Mock

#### `MockResponse`

``` purescript
type MockResponse = { statusCode :: Int, headers :: StrMap String, "data" :: String, cookies :: StrMap MockCookie }
```

#### `MockRequest`

``` purescript
newtype MockRequest
  = MockRequest { setHeader :: String -> String -> MockRequest, setBody :: String -> MockRequest, setBodyParam :: String -> String -> MockRequest, setRouteParam :: String -> String -> MockRequest, setCookie :: String -> String -> MockRequest, setSignedCookie :: String -> String -> MockRequest }
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
type TestExpress e = Aff (express :: EXPRESS, testOutput :: TESTOUTPUT | e)
```

#### `TestMockApp`

``` purescript
type TestMockApp e = ReaderT Application (TestExpress e) Unit
```

#### `createMockApp`

``` purescript
createMockApp :: forall e. Eff (express :: EXPRESS, testOutput :: TESTOUTPUT | e) Application
```

#### `createMockRequest`

``` purescript
createMockRequest :: forall e. String -> String -> ExpressM e MockRequest
```

#### `testExpress`

``` purescript
testExpress :: forall e. String -> TestMockApp e -> TestSuite (express :: EXPRESS, testOutput :: TESTOUTPUT | e)
```

#### `setupMockApp`

``` purescript
setupMockApp :: forall e. TestApp e -> TestMockApp e
```

#### `sendRequest`

``` purescript
sendRequest :: forall e. Method -> String -> (MockRequest -> MockRequest) -> (MockResponse -> TestMockApp e) -> TestMockApp e
```

#### `sendError`

``` purescript
sendError :: forall e. Method -> String -> String -> (MockResponse -> TestMockApp e) -> TestMockApp e
```

#### `assertMatch`

``` purescript
assertMatch :: forall a e. Show a => Eq a => String -> a -> a -> Test e
```

#### `assertInApp`

``` purescript
assertInApp :: forall e. (Reporter e -> TestApp e) -> TestMockApp e
```

#### `assertStatusCode`

``` purescript
assertStatusCode :: forall e. Int -> MockResponse -> TestMockApp e
```

#### `assertHeader`

``` purescript
assertHeader :: forall e. String -> Maybe String -> MockResponse -> TestMockApp e
```

#### `assertData`

``` purescript
assertData :: forall e. String -> MockResponse -> TestMockApp e
```

#### `assertCookieValue`

``` purescript
assertCookieValue :: forall e. String -> Maybe String -> MockResponse -> TestMockApp e
```

#### `setTestHeader`

``` purescript
setTestHeader :: forall e. String -> Handler e
```

#### `assertTestHeader`

``` purescript
assertTestHeader :: forall e. Maybe String -> MockResponse -> TestMockApp e
```


