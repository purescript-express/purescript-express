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

foreign import createMockMiddleware :: Fn1 Application (Fn3 Request Response (ExpressM Unit) (ExpressM Unit))

assertProperty :: forall a. (Show a, Eq a, IsForeign a) => String -> Maybe a -> TestApp
assertProperty name expected = do
    actual <- lift $ getProp name
    assertMatch ("Property '" ++ name ++ "'") expected actual

testValue = "TestValue"

testError = "Error"

sendTestRequest method url = do
    let request = createMockRequest method url
    liftEff $ request.setHeader "X-Test-Value-To-Return" testValue
    sendRequest request

sendTestError = do
    let request = createMockRequest "GET" "http://example/com"
    liftEff $ request.setHeader "X-Test-Value-To-Return" testValue
    sendError request testError

testApplicationGetProp = testApp "Application.getProp" $ do
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

testApplicationSetProp = testApp "Application.setProp" $ do
    assertProperty "notExistingYet" (Nothing :: Maybe String)
    lift $ setProp "notExistingYet" "nowItIsHere"
    assertProperty "notExistingYet" (Just "nowItIsHere")

testApplicationUse = testApp "Application.use" $ do
    lift $ use $ do
        Just value <- getRequestHeader "X-Test-Value-To-Return"
        setResponseHeader "X-Use-Handler" value
    response <- sendTestRequest "GET" "http://example.com/"
    assertHeader response "X-Use-Handler" $ Just testValue

testApplicationUseOnError = testApp "Application.useOnError" $ do
    lift $ useOnError $ \error -> do
        Just value <- getRequestHeader "X-Test-Value-To-Return"
        setResponseHeader "X-Use-On-Error-Handler" (value ++ message error)
    response <- sendTestError
    assertHeader response "X-Use-On-Error-Handler" $ Just (testValue ++ testError)

testApplicationUseExternal = testApp "Application.useExternal" $ do
    getMockApp >>= lift <<< useExternal <<< runFn1 createMockMiddleware
    response <- sendTestRequest "GET" "http://example.com/"
    assertHeader response "X-Mock-Middleware" $ Just testValue

testApplicationUseAt = testApp "Application.useAt" $ do
    lift $ useAt "/some/path" $ do
        Just value <- getRequestHeader "X-Test-Value-To-Return"
        setResponseHeader "X-Use-At-Handler" value
    response <- sendTestRequest "GET" "http://example.com/"
    assertHeader response "X-Use-At-Handler" Nothing
    response <- sendTestRequest "GET" "http://example.com/some/path"
    assertHeader response "X-Use-At-Handler" $ Just testValue

testSuite = do
    testApplicationGetProp
    testApplicationSetProp
    testApplicationUse
    testApplicationUseOnError
    testApplicationUseExternal
    testApplicationUseAt

