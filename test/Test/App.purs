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

foreign import mockMiddleware :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

assertProperty :: forall a e. (Show a, Eq a, IsForeign a) =>
    String -> Maybe a -> TestMockApp (express :: Express | e)
assertProperty name expected = assertInApp $ \report -> do
    actual <- getProp name
    let message = "Property '" ++ name ++ "' does not match: \
        \Expected [ " ++ show expected ++ " ] Got [ " ++ show actual ++ " ]"
    liftEff $ if expected == actual
                then report success
                else report $ failure message

testApplicationGetProp = testExpress "Application.getProp" $ do
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

testApplicationSetProp = testExpress "Application.setProp" $ do
    assertProperty "notExistingYet" (Nothing :: Maybe String)
    setupMockApp $ setProp "notExistingYet" "nowItIsHere"
    assertProperty "notExistingYet" (Just "nowItIsHere")

testValue = "TestValue"
testRequestHeader = "X-Test-Request-Header"
testResponseHeader = "X-Test-Response-Header"

sendTestRequest :: forall e.
    Method
    -> String
    -> (MockResponse -> TestMockApp (express :: Express | e))
    -> TestMockApp (express :: Express | e)
sendTestRequest method url testResponse = do
    request <- liftEff $ createMockRequest (show method) url
    liftEff $ request.setHeader testRequestHeader testValue
    sendRequest request testResponse

sendTestError :: forall e.
    (MockResponse -> TestMockApp (express :: Express | e))
    -> TestMockApp (express :: Express | e)
sendTestError testResponse = do
    request <- liftEff $ createMockRequest "GET" "http://example.com"
    sendError request testValue testResponse

setTestResponseHeader :: Handler
setTestResponseHeader = do
    Just value <- getRequestHeader testRequestHeader
    setResponseHeader testResponseHeader value

assertTestResponseHeader :: forall e. MockResponse -> TestMockApp e
assertTestResponseHeader response =
    assertHeader response testResponseHeader $ Just testValue

assertNoTestResponseHeader :: forall e. MockResponse -> TestMockApp e
assertNoTestResponseHeader response =
    assertHeader response testResponseHeader Nothing

testApplicationUse = testExpress "Application.use" $ do
    setupMockApp $ use setTestResponseHeader
    sendTestRequest GET "http://example.com/" assertTestResponseHeader

testApplicationUseOnError = testExpress "Application.useOnError" $ do
    setupMockApp $ useOnError $ \error -> do
        setResponseHeader testResponseHeader $ message error
    sendTestError assertTestResponseHeader

testApplicationUseExternal = testExpress "Application.useExternal" $ do
    setupMockApp $ useExternal mockMiddleware
    sendTestRequest GET "http://example.com/" assertTestResponseHeader

testApplicationUseAt = testExpress "Application.useAt" $ do
    setupMockApp $ useAt "/some/path" setTestResponseHeader
    sendTestRequest GET "http://example.com/" assertNoTestResponseHeader
    sendTestRequest GET "http://example.com/some/path" assertTestResponseHeader

testApplicationUseOnParam = testExpress "Application.useOnParam" $ do
    setupMockApp $ useOnParam "param" (setResponseHeader testResponseHeader)
    sendTestRequest GET "http://example.com/some/path" assertNoTestResponseHeader
    sendTestRequestWithParam assertTestResponseHeader
  where
    sendTestRequestWithParam testResponse = do
        request <- liftEff $ createMockRequest "GET" "http://example.com/"
        liftEff $ request.setRouteParam "param" testValue
        sendRequest request testResponse

testApplicationHttpMethod method = testExpress ("Application." ++ show method) $ do
    setupMockApp $ http method "/" setTestResponseHeader
    sendTestRequest (CustomMethod "") "http://example.com/" assertNoTestResponseHeader
    sendTestRequest method "http://example.com/" assertTestResponseHeader

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
