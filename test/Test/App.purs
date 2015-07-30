module Test.App (testSuite) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
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

type TestExpress e a = Eff ( express :: Express, testOutput :: TestOutput | e ) a

type AssertionExpress e = Assertion ( express :: Express, testOutput :: TestOutput | e)

testGetProperty ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> (TestResult -> TestExpress e Unit) -> TestExpress e Unit
testGetProperty mockApp property expected callback = do
    actual <- intlAppGetProp mockApp property
    if (actual == expected)
        then callback success
        else callback $ failure (show actual ++ " should be equal " ++ show expected)

assertProperty ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> AssertionExpress e
assertProperty mockApp property expected =
    testFn (testGetProperty mockApp property expected)

toString :: forall a. a -> String
toString = unsafeCoerce

testBindHttp ::
    forall e. Application -> Method -> String -> AssertionExpress e
testBindHttp mockApp method route = do
    let propertyName = "bindHttpProperty"
        expected = (show method) ++ "::" ++ route ++ "::OK"
        handler req resp next = do
            let val = toString req ++ "::" ++ toString resp ++ "::" ++ toString next
            intlAppSetProp mockApp propertyName val
    liftEff $ intlAppHttp mockApp (show method) route handler
    assertProperty mockApp propertyName (Just expected)

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
        testBindHttp mockApp ALL "/some/path"
        testBindHttp mockApp GET "/some/path"
        testBindHttp mockApp POST "/some/path"
        testBindHttp mockApp PUT "/some/path"
        testBindHttp mockApp DELETE "/some/path"
        testBindHttp mockApp OPTIONS "/some/path"
        testBindHttp mockApp HEAD "/some/path"
        testBindHttp mockApp TRACE "/some/path"
