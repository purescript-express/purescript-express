module Test.App (testSuite) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Foreign.Class
import Data.Foreign.Null
import Data.Foreign.Undefined
import Data.Function
import Data.Maybe
import Node.Express.Types
import Node.Express.Internal.App
import Prelude
import Test.Unit
import Test.Unit.Console
import Unsafe.Coerce

foreign import createMockApp :: Fn0 Application
foreign import registerCall :: Fn2 Application String Unit
foreign import getCallData :: Fn1 Application String

type TestExpress e a = Eff ( express :: Express, testOutput :: TestOutput | e ) a

type AssertionExpress e = Assertion ( express :: Express, testOutput :: TestOutput | e)

toString :: forall a. a -> String
toString = unsafeCoerce

testGetProperty ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> (TestResult -> TestExpress e Unit) -> TestExpress e Unit
testGetProperty mockApp property expected callback = do
    actual <- intlAppGetProp mockApp property
    let errorMessage = show actual ++ " should be equal " ++ show expected
        result = if (actual == expected) then success else failure errorMessage
    callback result

assertProperty ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> AssertionExpress e
assertProperty mockApp property expected =
    testFn (testGetProperty mockApp property expected)

assertApplicationCall ::
    forall e. Application -> String -> (Application -> ExpressM Unit) -> AssertionExpress e
assertApplicationCall mockApp expectedVal callFn = do
    liftEff $ callFn mockApp
    let callData = runFn1 getCallData mockApp
        errorMessage = callData ++ " should be equal to " ++ expectedVal
    assert errorMessage (callData == expectedVal)

genHandler :: Application -> Request -> Response -> ExpressM Unit -> ExpressM Unit
genHandler mockApp req resp next = do
    let val = toString req ++ "::" ++ toString resp ++ "::" ++ toString next
    return $ runFn2 registerCall mockApp val

genErrorHandler :: Application -> Error -> Request -> Response -> ExpressM Unit -> ExpressM Unit
genErrorHandler mockApp err req resp next = do
    let val = toString err ++ "::" ++ toString req ++ "::" ++ toString resp ++ "::" ++ toString next
    return $ runFn2 registerCall mockApp val

callBindHttp :: Method -> String -> Application -> ExpressM Unit
callBindHttp method route mockApp =
    liftEff $ intlAppHttp mockApp (show method) route (genHandler mockApp)

callUseMiddleware :: Application -> ExpressM Unit
callUseMiddleware mockApp =
    liftEff $ intlAppUse mockApp (genHandler mockApp)

callUseMiddlewareOnError :: Application -> ExpressM Unit
callUseMiddlewareOnError mockApp =
    liftEff $ intlAppUseOnError mockApp (genErrorHandler mockApp)

testSuite = do
    let mockApp = runFn0 createMockApp
    test "Internal.App.getProperty" do
        assertProperty mockApp "stringProperty" (Just "string")
        -- Uncomment when there is IsForeign Int instance
        -- assertProperty mockApp "intProperty" (Just 42)
        assertProperty mockApp "floatProperty" (Just 100.1)
        assertProperty mockApp "booleanProperty" (Just true)
        assertProperty mockApp "booleanFalseProperty" (Just false)
        assertProperty mockApp "arrayProperty" (Just ["a", "b", "c"])
        assertProperty mockApp "emptyArrayProperty" (Just [] :: Maybe (Array String))
    test "Internal.App.setProperty" do
        assertProperty mockApp "testProperty" (Nothing :: Maybe String)
        liftEff $ intlAppSetProp mockApp "testProperty" "OK"
        assertProperty mockApp "testProperty" (Just "OK")
    test "Internal.App.bindHttp" do
        let assertBindHttpCall method route =
            assertApplicationCall mockApp
                (show method ++ "::" ++ route ++ "::OK")
                (callBindHttp method route)
        assertBindHttpCall ALL "/some/path"
        assertBindHttpCall GET "/some/path"
        assertBindHttpCall POST "/some/path"
        assertBindHttpCall PUT "/some/path"
        assertBindHttpCall DELETE "/some/path"
        assertBindHttpCall OPTIONS "/some/path"
        assertBindHttpCall HEAD "/some/path"
        assertBindHttpCall TRACE "/some/path"
    test "Internal.App.useMiddleware" do
        assertApplicationCall mockApp "request::response::next" callUseMiddleware
    test "Internal.App.useMiddlewareOnError" do
        assertApplicationCall mockApp "error::request::response::next" callUseMiddlewareOnError


