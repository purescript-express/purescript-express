module Node.Express.Test.Mock
    ( MockResponse(..)
    , MockRequest(..)
    , MockCookie(..)
    , setRequestHeader
    , setRouteParam
    , setBody
    , setBody'
    , setBodyParam
    , setRequestCookie
    , setRequestSignedCookie
    , TestExpress(..)
    , TestMockApp(..)
    , createMockApp
    , createMockRequest
    , testExpress
    , setupMockApp
    , sendRequest
    , sendError
    , assertMatch
    , assertInApp
    , assertStatusCode
    , assertHeader
    , assertData
    , assertCookieValue
    , setTestHeader
    , assertTestHeader
    ) where

import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Reader.Trans
import Foreign (Foreign)
import Foreign.Class (encode)
import Foreign.Object (Object, lookup)
import Data.Maybe (Maybe)
import Node.Express.App (App, apply)
import Node.Express.Handler (Handler)
import Node.Express.Types (Application, Method)
import Node.Express.Response (setResponseHeader)
import Test.Unit (Test, TestSuite, test)
import Test.Unit.Assert (assert)
import Effect.Aff (Aff, launchAff)

import Prelude (class Eq, class Show, Unit, bind, map, pure, show, unit, ($), (<>), (==), (>>=))

type MockCookie =
    { name    :: String
    , value   :: String
    , options :: String
    }

type MockResponse =
    { statusCode  :: Int
    , headers     :: Object String
    , data        :: String
    , cookies     :: Object MockCookie
    }

newtype MockRequest = MockRequest
    { setHeader :: String -> String -> MockRequest
    , setBody :: Foreign -> MockRequest
    , setBodyParam :: String -> String -> MockRequest
    , setRouteParam :: String -> String -> MockRequest
    , setCookie :: String -> String -> MockRequest
    , setSignedCookie :: String -> String -> MockRequest
    }

setRequestHeader :: String -> String -> MockRequest -> MockRequest
setRequestHeader name value (MockRequest r) = r.setHeader name value

setBody :: String -> MockRequest -> MockRequest
setBody value (MockRequest r) = r.setBody $ encode value

setBody' :: Foreign -> MockRequest -> MockRequest
setBody' value (MockRequest r) = r.setBody value

setBodyParam :: String -> String -> MockRequest -> MockRequest
setBodyParam name value (MockRequest r) = r.setBodyParam name value

setRouteParam :: String -> String -> MockRequest -> MockRequest
setRouteParam name value (MockRequest r) = r.setRouteParam name value

setRequestCookie :: String -> String -> MockRequest -> MockRequest
setRequestCookie name value (MockRequest r) = r.setCookie name value

setRequestSignedCookie :: String -> String -> MockRequest -> MockRequest
setRequestSignedCookie name value (MockRequest r) = r.setSignedCookie name value

foreign import createMockApp :: Effect Application
foreign import createMockRequest :: String -> String -> Effect MockRequest
foreign import sendMockRequest ::
    Application -> MockRequest -> Effect MockResponse
foreign import sendMockError ::
    Application -> MockRequest -> String -> Effect MockResponse

type TestExpress = Aff
type TestMockApp = ReaderT Application TestExpress Unit
type TestApp = App

testExpress ::
    String
    -> TestMockApp
    -> TestSuite
testExpress testName assertions = test testName $ do
    mockApp <- liftEffect $ createMockApp
    runReaderT assertions mockApp

setupMockApp :: TestApp -> TestMockApp
setupMockApp app = do
    mockApp <- ask
    liftEffect $ apply app mockApp

sendRequest ::
    Method
    -> String
    -> (MockRequest -> MockRequest)
    -> (MockResponse -> TestMockApp)
    -> TestMockApp
sendRequest method url setupRequest testResponse = do
    app <- ask
    request <- liftEffect $ map setupRequest $ createMockRequest (show method) url
    response <- liftEffect $ sendMockRequest app request
    testResponse response

sendError ::
    Method
    -> String
    -> String
    -> (MockResponse -> TestMockApp)
    -> TestMockApp
sendError method url error testResponse = do
    app <- ask
    request <- liftEffect $ createMockRequest (show method) url
    response <- liftEffect $ sendMockError app request error
    testResponse response

assertMatch :: forall a. Show a => Eq a => String -> a -> a -> Test
assertMatch what expected actual = do
    let message = what <> " does not match: \
        \Expected [ " <> show expected <> " ], Got [ " <> show actual <> " ]"
    assert message (expected == actual)

type Reporter = Test -> Effect Unit

assertInApp :: (Reporter -> TestApp) -> TestMockApp
assertInApp assertion = do
    mockApp <- ask
    let reporter result = launchAff result >>= \_ -> pure unit
    liftEffect $ apply (assertion reporter) mockApp

assertStatusCode :: Int -> MockResponse -> TestMockApp
assertStatusCode expected response =
    lift $ assertMatch "Status code" expected response.statusCode

assertHeader :: String -> Maybe String -> MockResponse -> TestMockApp
assertHeader name expected response = do
    let actual = lookup name response.headers
    lift $ assertMatch ("Header '" <> name <> "'") expected actual

assertData :: String -> MockResponse -> TestMockApp
assertData expected response =
    lift $ assertMatch "Response data" expected response.data

assertCookieValue :: String -> Maybe String -> MockResponse -> TestMockApp
assertCookieValue name expected response = do
    let actual = map (\r -> r.value) $ lookup name response.cookies
    lift $ assertMatch ("Cookie '" <> name <> "'") expected actual

setTestHeader :: String -> Handler
setTestHeader = setResponseHeader "X-Test-Response-Header"

assertTestHeader :: Maybe String -> MockResponse -> TestMockApp
assertTestHeader value response = assertHeader "X-Test-Response-Header" value response
