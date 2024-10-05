module Test.App (testSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Foreign.Generic.Class (class Decode)
import Foreign.Object (Object)
import Node.Express.App (getProp, http, setProp, use, useAt, useAtExternal, useExternal, useOnError, useOnParam)
import Node.Express.Test.Mock (MockResponse, TestMockApp, assertInApp, assertTestHeader, sendError, sendRequest, setRouteParam, setTestHeader, setupMockApp, testExpress)
import Node.Express.Types (Application, Method(..), Middleware)
import Test.Unit (TestF, failure, success, suite)

foreign import mockMiddleware
  :: String -> Middleware

assertProperty
  :: forall a
   . Show a
  => Eq a
  => Decode a
  => String
  -> Maybe a
  -> TestMockApp
assertProperty name expected = assertInApp $ \report -> do
  actual <- getProp name
  let
    message = "Property '" <> name
      <>
        "' does not match: \
        \Expected [ "
      <> show expected
      <> " ] Got [ "
      <> show actual
      <> " ]"
  liftEffect $
    if expected == actual then report success
    else report $ failure message

testApplicationGetProp :: Free TestF Unit
testApplicationGetProp = testExpress "getProp" $ do
  assertProperty "string" (Just "string")
  assertProperty "emptyString" (Just "")
  assertProperty "fortyTwo" (Just 42)
  assertProperty "zeroInt" (Just 0)
  assertProperty "hundredPointOne" (Just 100.1)
  assertProperty "zeroFloat" (Just 0.0)
  assertProperty "trueBoolean" (Just true)
  assertProperty "falseBoolean" (Just false)
  assertProperty "abcArray" (Just [ "a", "b", "c" ])
  assertProperty "emptyArray" (Just [] :: Maybe (Array Int))

testApplicationSetProp :: Free TestF Unit
testApplicationSetProp = testExpress "setProp" $ do
  assertProperty "notExistingYet" (Nothing :: Maybe String)
  setupMockApp $ setProp "notExistingYet" "nowItIsHere"
  assertProperty "notExistingYet" (Just "nowItIsHere")

testValue :: String
testValue = "TestValue"

sendTestRequest
  :: Method
  -> String
  -> (MockResponse -> TestMockApp)
  -> TestMockApp
sendTestRequest method url testResponse =
  sendRequest method url (\x -> x) testResponse

sendTestError :: (MockResponse -> TestMockApp) -> TestMockApp
sendTestError testResponse =
  sendError GET "http://example.com/" testValue testResponse

assertTestHeaderExists
  :: { cookies ::
         Object
           { name :: String
           , options :: String
           , value :: String
           }
     , data :: String
     , headers :: Object String
     , statusCode :: Int
     }
  -> ReaderT Application Aff Unit
assertTestHeaderExists = assertTestHeader $ Just testValue

assertTestHeaderAbsent
  :: { cookies ::
         Object
           { name :: String
           , options :: String
           , value :: String
           }
     , data :: String
     , headers :: Object String
     , statusCode :: Int
     }
  -> ReaderT Application Aff Unit
assertTestHeaderAbsent = assertTestHeader Nothing

testApplicationUse :: Free TestF Unit
testApplicationUse = testExpress "use" $ do
  setupMockApp $ use $ setTestHeader testValue
  sendTestRequest GET "http://example.com/" assertTestHeaderExists

testApplicationUseOnError :: Free TestF Unit
testApplicationUseOnError = testExpress "useOnError" $ do
  setupMockApp $ useOnError $ \error -> setTestHeader $ message error
  sendTestRequest GET "http://example.com/" assertTestHeaderAbsent
  sendTestError assertTestHeaderExists

testApplicationUseExternal :: Free TestF Unit
testApplicationUseExternal = testExpress "useExternal" $ do
  setupMockApp $ useExternal (mockMiddleware testValue)
  sendTestRequest GET "http://example.com/" assertTestHeaderExists

testApplicationUseAt :: Free TestF Unit
testApplicationUseAt = testExpress "useAt" $ do
  setupMockApp $ useAt "/some/path" $ setTestHeader testValue
  sendTestRequest GET "http://example.com/" assertTestHeaderAbsent
  sendTestRequest GET "http://example.com/some/path" assertTestHeaderExists

testApplicationUseAtExternal :: Free TestF Unit
testApplicationUseAtExternal = testExpress "useAtExternal" $ do
  setupMockApp $ useAtExternal "/some/path" (mockMiddleware testValue)
  sendTestRequest GET "http://example.com/" assertTestHeaderAbsent
  sendTestRequest GET "http://example.com/some/path" assertTestHeaderExists

testApplicationUseOnParam :: Free TestF Unit
testApplicationUseOnParam = testExpress "useOnParam" $ do
  setupMockApp $ useOnParam "param" setTestHeader
  sendTestRequest GET "http://example.com/some/path" assertTestHeaderAbsent
  sendRequest GET "http://example/com" withRouteParam $ assertTestHeaderExists
  where
  withRouteParam = setRouteParam "param" testValue

testApplicationHttpMethod :: Method -> Free TestF Unit
testApplicationHttpMethod method = testExpress (show method) $ do
  setupMockApp $ http method "/" $ setTestHeader testValue
  sendTestRequest (CustomMethod "") "http://example.com/" assertTestHeaderAbsent
  sendTestRequest method "http://example.com/" assertTestHeaderExists

testSuite :: Free TestF Unit
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
