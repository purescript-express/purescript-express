module Test.App (testSuite) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Reader.Trans
import Data.Array (zip, zipWith, foldM, length, filter, null)
import Data.Foreign.Class
import Data.Foreign.Null
import Data.Foreign.Undefined
import Data.Function
import Data.Maybe
import qualified Data.StrMap as StrMap
import Data.Tuple
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import Prelude hiding (apply)
import Test.Unit
import Test.Unit.Console
import Unsafe.Coerce

type MockResponse = {
    status      :: Int,
    contentType :: String,
    headers     :: StrMap.StrMap String,
    data        :: String
}

type MockRequest = {
    setHeader :: forall e. String -> String -> Eff e Unit,
    setBodyParam :: forall e. String -> String -> Eff e Unit
}

foreign import createMockApp :: Fn0 Application
foreign import createMockMiddleware :: Fn1 Application (Fn3 Request Response (ExpressM Unit) (ExpressM Unit))
foreign import createMockRequest :: String -> String -> MockRequest
foreign import sendMockRequest :: forall e. Application -> MockRequest -> Eff e MockResponse
foreign import sendMockError :: forall e. Application -> MockRequest -> String -> Eff e MockResponse

type TestAppM = ReaderT (Tuple Application (TestResult -> App)) AppM
type TestApp = TestAppM Unit

testApp :: forall e.
           String -> TestApp ->
           Assertion ( express :: Express, testOutput :: TestOutput | e)
testApp testName genApp = test testName $ do testFn tester where
    tester callback =
        let mockApp = runFn0 createMockApp
            app = runReaderT genApp $ Tuple mockApp (liftEff <<< callback) in
        liftEff $ apply app mockApp

-- TODO: report type inference issue
appAndReporter :: TestAppM (Tuple Application (TestResult -> App))
appAndReporter = ask

report :: TestResult -> TestApp
report message = do
    reporter <- map snd appAndReporter
    lift $ reporter message

mockApp :: TestAppM Application
mockApp = map fst appAndReporter

sendRequest :: MockRequest -> TestAppM MockResponse
sendRequest request = mockApp >>= (liftEff <<< (flip sendMockRequest) request)

sendError :: MockRequest -> String -> TestAppM MockResponse
sendError request error = do
    app <- mockApp
    liftEff $ sendMockError app request error

assertMatch :: forall a. (Show a, Eq a) => String -> Maybe a -> Maybe a -> TestApp
assertMatch what expected actual = do
    let message = what ++ " does not match: \
        \Expected [ " ++ show expected ++ " ], Got [ " ++ show actual ++ " ]"
    if expected == actual
        then report success
        else report $ failure message

assertProperty :: forall a. (Show a, Eq a, IsForeign a) => String -> Maybe a -> TestApp
assertProperty name expected = do
    actual <- lift $ getProp name
    assertMatch ("Property '" ++ name ++ "'") expected actual

assertHeader :: MockResponse -> String -> Maybe String -> TestApp
assertHeader response name expected = do
    let actual = StrMap.lookup name response.headers
    assertMatch ("Header '" ++ name ++ "'") expected actual

testSuite = do
    testApp "Application.getProp" $ do
        assertProperty "string" (Just "string")
        assertProperty "emptyString" (Just "")
        assertProperty "fortyTwo" (Just 42)
        assertProperty "zeroInt" (Just 0)
        assertProperty "hundredPointOne" (Just 100.1)
        assertProperty "zeroFloat" (Just 0.0)
        assertProperty "trueBoolean" (Just true)
        assertProperty "falseBoolean" (Just false)
        assertProperty "abcArray" (Just ["a", "b", "c"])
        assertProperty "emptyArray" (Just [] :: Maybe (Array Int))
    testApp "Application.setProp" $ do
        assertProperty "notExistingYet" (Nothing :: Maybe String)
        lift $ setProp "notExistingYet" "nowItIsHere"
        assertProperty "notExistingYet" (Just "nowItIsHere")
    testApp "Application.use" $ do
        lift $ use $ do
            Just value <- getRequestHeader "X-Test-Value-To-Return"
            setResponseHeader "X-Use-Handler" value
        response <- sendTestRequest
        assertHeader response "X-Use-Handler" $ Just testValue
    testApp "Application.useOnError" $ do
        lift $ useOnError $ \error -> do
            Just value <- getRequestHeader "X-Test-Value-To-Return"
            setResponseHeader "X-Use-On-Error-Handler" (value ++ message error)
        response <- sendTestError
        assertHeader response "X-Use-On-Error-Handler" $ Just (testValue ++ testError)
    testApp "Application.useExternal" $ do
        mockApp >>= lift <<< useExternal <<< runFn1 createMockMiddleware
        response <- sendTestRequest
        assertHeader response "X-Mock-Middleware" $ Just testValue
  where
    testValue = "TestValue"
    testError = "Error"
    sendTestRequest = do
        let request = createMockRequest "GET" "http://example/com"
        liftEff $ request.setHeader "X-Test-Value-To-Return" testValue
        sendRequest request
    sendTestError = do
        let request = createMockRequest "GET" "http://example/com"
        liftEff $ request.setHeader "X-Test-Value-To-Return" testValue
        sendError request testError

