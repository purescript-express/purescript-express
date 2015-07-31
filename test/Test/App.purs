module Test.App (testSuite) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Array (zip, zipWith, foldM, length, filter, null)
import Data.Foreign.Class
import Data.Foreign.Null
import Data.Foreign.Undefined
import Data.Function
import Data.Maybe
import Data.Tuple
import Node.Express.Types
import Node.Express.Internal.App
import Prelude
import Test.Unit
import Test.Unit.Console
import Unsafe.Coerce

type FnCall = { name :: String, arguments :: Array String }

fnCall :: String -> Array String -> FnCall
fnCall name args = { name: name, arguments: args }

foreign import putCall :: Fn2 Application FnCall Unit
foreign import getCalls :: Fn1 Application (Array FnCall)
foreign import clearCalls :: Fn1 Application Unit

foreign import createMockApp :: Fn0 Application
foreign import createMockMiddleware :: Fn1 Application (Fn3 Request Response (ExpressM Unit) (ExpressM Unit))

type TestExpress e a = Eff ( express :: Express, testOutput :: TestOutput | e ) a
type AssertionExpress e = Assertion ( express :: Express, testOutput :: TestOutput | e)

toString :: forall a. a -> String
toString = unsafeCoerce

assertMatch :: forall a e. (Show a, Eq a) => String -> a -> a -> Assertion e
assertMatch what expected actual = do
    let errorMessage = what ++ " mismatch: Expected [ " ++ show expected ++ " ], Got [ " ++ show actual ++ " ]"
    assert errorMessage (expected == actual)

assertProperty ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> AssertionExpress e
assertProperty mockApp property expected = do
    actual <- liftEff $ intlAppGetProp mockApp property
    assertMatch "Property" expected actual

assertCalls :: forall e. Application -> Array FnCall -> Assertion e
assertCalls mockApp expectedCalls = do
    let actualCalls = runFn1 getCalls mockApp
    assertMatch "Calls size" (length expectedCalls) (length actualCalls)
    foldM assertCallsMatch unit (zip expectedCalls actualCalls)
  where
    assertCallsMatch _ (Tuple expected actual) = do
        assertMatch "Call name" expected.name actual.name
        assertArgumentsMatch expected.arguments actual.arguments
    assertArgumentsMatch expected actual = do
        let lengthMatch = length expected == length actual
            argsMatch = null $ filter unmatched (zip expected actual)
            unmatched (Tuple e a) = e /= a
            errorMessage = "Call arguments mismatch: Expected [ " ++ show expected ++ " ], Got [ " ++ show actual ++ " ]"
        assert errorMessage (lengthMatch && argsMatch)

genHandler :: Application -> Request -> Response -> ExpressM Unit -> ExpressM Unit
genHandler mockApp req resp next =
    return $ runFn2 putCall mockApp $
        fnCall "handler" [toString req, toString resp, toString next]

genErrorHandler :: Application -> Error -> Request -> Response -> ExpressM Unit -> ExpressM Unit
genErrorHandler mockApp err req resp next =
    return $ runFn2 putCall mockApp $
        fnCall "handler" [toString err, toString req, toString resp, toString next]

testBindHttp :: forall e. Application -> Method -> String -> AssertionExpress e
testBindHttp mockApp method route = do
    return $ runFn1 clearCalls mockApp
    liftEff $ intlAppHttp mockApp (show method) route (genHandler mockApp)
    assertCalls mockApp [
        fnCall (show method) [route],
        fnCall "handler" ["request", "response", "next"]
    ]

testMiddleware :: forall e h.
    Application
    -> (Application -> h -> ExpressM Unit)
    -> (Application -> h)
    -> Array FnCall
    -> AssertionExpress e
testMiddleware mockApp useFn handlerGenFn expectedCalls = do
    return $ runFn1 clearCalls mockApp
    liftEff $ useFn mockApp (handlerGenFn mockApp)
    assertCalls mockApp expectedCalls

testSuite = do
    let mockApp = runFn0 createMockApp
    test "Internal.App.getProperty" do
        assertProperty mockApp "stringProperty" (Just "string")
        -- FIXME: Uncomment when there is IsForeign Int instance
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
    test "Internal.App.useMiddleware" do
        testMiddleware mockApp intlAppUse genHandler
            [ fnCall "use" [], fnCall "handler" ["request", "response", "next"] ]
    test "Internal.App.useMiddlewareOnError" do
        testMiddleware mockApp intlAppUseOnError genErrorHandler
            [ fnCall "use" [], fnCall "handler" ["error", "request", "response", "next"] ]
    test "Internal.App.useExternalMiddleware" do
        testMiddleware mockApp intlAppUseExternal (runFn1 createMockMiddleware)
            [ fnCall "use" [], fnCall "handler" ["request", "response", "next"] ]
    test "Internal.App.useMiddlewareAt" do
        let route = "/some/path"
            intlAppUseAtWrapper a = intlAppUseAt a route
        testMiddleware mockApp intlAppUseAtWrapper genHandler
            [ fnCall "use" [route], fnCall "handler" ["request", "response", "next"] ]

