module Test.App (testSuite) where

import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Trans
import Data.Foreign.Class
import Data.Function
import Data.Maybe
import Node.Express.Types
import Node.Express.App hiding (apply)
import Node.Express.Handler
import Prelude
import Test.Mock
import Test.Unit
import Test.Unit.Console

foreign import createMockMiddleware :: Fn1 Application (Fn3 Request Response (ExpressM Unit) (ExpressM Unit))

assertProperty :: forall a. (Show a, Eq a, IsForeign a) => String -> Maybe a -> TestApp
assertProperty name expected = do
    actual <- lift $ getProp name
    assertMatch ("Property '" ++ name ++ "'") expected actual

testApplicationGetProp = do
    testProperty "string" (Just "string")
    testProperty "emptyString" (Just "")
    testProperty "fortyTwo" (Just 42)
    testProperty "zeroInt" (Just 0)
    testProperty "hundredPointOne" (Just 100.1)
    testProperty "zeroFloat" (Just 0.0)
    testProperty "trueBoolean" (Just true)
    testProperty "falseBoolean" (Just false)
    testProperty "abcArray" (Just ["a", "b", "c"])
    testProperty "emptyArray" (Just [] :: Maybe (Array Int))
  where
    testProperty :: forall a e. (IsForeign a, Eq a, Show a) =>
        String -> Maybe a -> Assertion ( express :: Express, testOutput :: TestOutput | e)
    testProperty name value =
        testApp ("Application.getProp." ++ name) $ assertProperty name value

testApplicationSetProp = do
    testApp "Application.setProp.1" $ do
        assertProperty "notExistingYet" (Nothing :: Maybe String)
    testApp "Application.setProp.2" $ do
        lift $ setProp "notExistingYet" "nowItIsHere"
        assertProperty "notExistingYet" (Just "nowItIsHere")

testValue = "TestValue"
testRequestHeader = "X-Test-Request-Header"
testResponseHeader = "X-Test-Response-Header"

sendTestRequest method url = do
    request <- liftEff $ do
        req <- createMockRequest method url
        req.setHeader testRequestHeader testValue
        return req
    sendRequest request

sendTestError = do
    request <- liftEff $ createMockRequest "GET" "http://example/com"
    sendError request testValue

setTestResponseHeader = do
    Just value <- getRequestHeader testRequestHeader
    setResponseHeader testResponseHeader value

assertTestResponseHeader response =
    assertHeader response testResponseHeader $ Just testValue

assertNoTestResponseHeader response =
    assertHeader response testResponseHeader Nothing

testApplicationUse = testApp "Application.use" $ do
    lift $ use setTestResponseHeader
    sendTestRequest "GET" "http://example.com/" >>= assertTestResponseHeader

testApplicationUseOnError = testApp "Application.useOnError" $ do
    lift $ useOnError $ \error -> do
        setResponseHeader testResponseHeader $ message error
    sendTestError >>= assertTestResponseHeader

testApplicationUseExternal = testApp "Application.useExternal" $ do
    getMockApp >>= lift <<< useExternal <<< runFn1 createMockMiddleware
    sendTestRequest "GET" "http://example.com/" >>= assertTestResponseHeader

testApplicationUseAt = do
    testApp "Application.useAt.1" $ do
        lift $ useAt "/some/path" setTestResponseHeader
        sendTestRequest "GET" "http://example.com/" >>= assertNoTestResponseHeader
    testApp "Application.useAt.2" $ do
        lift $ useAt "/some/path" setTestResponseHeader
        sendTestRequest "GET" "http://example.com/some/path" >>= assertTestResponseHeader

testApplicationUseOnParam = do
    testApp "Application.useOnParam.1" $ do
        lift $ useOnParam "param" (setResponseHeader testResponseHeader)
        sendTestRequest "GET" "http://example.com/some/path" >>= assertNoTestResponseHeader
    testApp "Application.useOnParam.2" $ do
        lift $ useOnParam "param" (setResponseHeader testResponseHeader)
        sendTestRequestWithParam >>= assertTestResponseHeader
  where
    sendTestRequestWithParam = do
        request <- liftEff $ do
            req <- createMockRequest "GET" "http://example.com/"
            req.setRouteParam "param" testValue
            return req
        sendRequest request

testApplicationHttpMethod method = do
    testApp ("Application." ++ show method ++ ".1") $ do
        lift $ http method "/" setTestResponseHeader
        sendTestRequest "" "http://example.com/" >>= assertNoTestResponseHeader
    testApp ("Application." ++ show method ++ ".2") $ do
        lift $ http method "/" setTestResponseHeader
        sendTestRequest (show method) "http://example.com/" >>= assertTestResponseHeader

testSuite = do
    testApplicationGetProp
    testApplicationSetProp
    testApplicationUse
    testApplicationUseOnError
    testApplicationUseExternal
    testApplicationUseAt
    testApplicationUseOnParam
    testApplicationHttpMethod GET
    testApplicationHttpMethod POST
    testApplicationHttpMethod PUT
    testApplicationHttpMethod DELETE
    testApplicationHttpMethod OPTIONS
    testApplicationHttpMethod HEAD
    testApplicationHttpMethod TRACE
