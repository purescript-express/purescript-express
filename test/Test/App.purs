module Test.App (testSuite) where

import Effect
import Effect.Class
import Effect.Exception
import Foreign.Class (class Decode)
import Control.Monad.Trans.Class
import Data.Function.Uncurried
import Data.Maybe
import Node.Express.Types
import Node.Express.App hiding (apply)
import Node.Express.Handler
import Node.Express.Test.Mock
import Prelude
import Test.Unit
import Test.Unit.Console

foreign import mockMiddleware ::
    String -> Fn3 Request Response (Effect Unit) (Effect Unit)

assertProperty :: forall a. Show a => Eq a => Decode a =>
    String -> Maybe a -> TestMockApp
assertProperty name expected = assertInApp $ \report -> do
    actual <- getProp name
    let message = "Property '" <> name <> "' does not match: \
        \Expected [ " <> show expected <> " ] Got [ " <> show actual <> " ]"
    liftEffect $ if expected == actual
                  then report success
                  else report $ failure message

testApplicationGetProp = testExpress "getProp" $ do
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

testApplicationSetProp = testExpress "setProp" $ do
    assertProperty "notExistingYet" (Nothing :: Maybe String)
    setupMockApp $ setProp "notExistingYet" "nowItIsHere"
    assertProperty "notExistingYet" (Just "nowItIsHere")

testValue = "TestValue"

sendTestRequest ::
    Method
    -> String
    -> (MockResponse -> TestMockApp)
    -> TestMockApp
sendTestRequest method url testResponse =
    sendRequest method url (\x -> x) testResponse

sendTestError :: (MockResponse -> TestMockApp) -> TestMockApp
sendTestError testResponse =
    sendError GET "http://example.com/" testValue testResponse

assertTestHeaderExists = assertTestHeader $ Just testValue
assertTestHeaderAbsent = assertTestHeader Nothing

testApplicationUse = testExpress "use" $ do
    setupMockApp $ use $ setTestHeader testValue
    sendTestRequest GET "http://example.com/" assertTestHeaderExists

testApplicationUseOnError = testExpress "useOnError" $ do
    setupMockApp $ useOnError $ \error -> setTestHeader $ message error
    sendTestRequest GET "http://example.com/" assertTestHeaderAbsent
    sendTestError assertTestHeaderExists

testApplicationUseExternal = testExpress "useExternal" $ do
    setupMockApp $ useExternal (mockMiddleware testValue)
    sendTestRequest GET "http://example.com/" assertTestHeaderExists

testApplicationUseAt = testExpress "useAt" $ do
    setupMockApp $ useAt "/some/path" $ setTestHeader testValue
    sendTestRequest GET "http://example.com/" assertTestHeaderAbsent
    sendTestRequest GET "http://example.com/some/path" assertTestHeaderExists

testApplicationUseAtExternal = testExpress "useAtExternal" $ do
    setupMockApp $ useAtExternal "/some/path" (mockMiddleware testValue)
    sendTestRequest GET "http://example.com/" assertTestHeaderAbsent
    sendTestRequest GET "http://example.com/some/path" assertTestHeaderExists

testApplicationUseOnParam = testExpress "useOnParam" $ do
    setupMockApp $ useOnParam "param" setTestHeader
    sendTestRequest GET "http://example.com/some/path" assertTestHeaderAbsent
    sendRequest GET "http://example/com" withRouteParam $ assertTestHeaderExists
  where
    withRouteParam = setRouteParam "param" testValue

testApplicationHttpMethod method = testExpress (show method) $ do
    setupMockApp $ http method "/" $ setTestHeader testValue
    sendTestRequest (CustomMethod "") "http://example.com/" assertTestHeaderAbsent
    sendTestRequest method "http://example.com/" assertTestHeaderExists

testSuite = suite "Application" do
    testApplicationGetProp
    testApplicationSetProp
    testApplicationUse
    testApplicationUseOnError
    testApplicationUseExternal
    testApplicationUseAt
    testApplicationUseAtExternal
    testApplicationUseOnParam
    testApplicationHttpMethod GET
    testApplicationHttpMethod POST
    testApplicationHttpMethod PUT
    testApplicationHttpMethod DELETE
    testApplicationHttpMethod OPTIONS
    testApplicationHttpMethod HEAD
    testApplicationHttpMethod TRACE
