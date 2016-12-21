module Test.Mock
    ( MockResponse(..)
    , MockRequest(..)
    , MockCookie(..)
    , setRequestHeader
    , setRouteParam
    , setBody
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

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Cont.Trans
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Data.Maybe
import Data.Tuple
import Node.Express.App
import Node.Express.Handler
import Node.Express.Types
import Node.Express.Response
import Test.Unit
import Test.Unit.Console
import Test.Unit.Assert
import Data.StrMap as StrMap
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Function hiding (apply)
import Prelude hiding (apply)

type MockCookie =
    { name    :: String
    , value   :: String
    , options :: String
    }

type MockResponse =
    { statusCode  :: Int
    , headers     :: StrMap.StrMap String
    , data        :: String
    , cookies     :: StrMap.StrMap MockCookie
    }

newtype MockRequest = MockRequest
    { setHeader :: String -> String -> MockRequest
    , setBody :: String -> MockRequest
    , setBodyParam :: String -> String -> MockRequest
    , setRouteParam :: String -> String -> MockRequest
    , setCookie :: String -> String -> MockRequest
    , setSignedCookie :: String -> String -> MockRequest
    }

setRequestHeader :: String -> String -> MockRequest -> MockRequest
setRequestHeader name value (MockRequest r) = r.setHeader name value

setBody :: String -> MockRequest -> MockRequest
setBody value (MockRequest r) = r.setBody value

setBodyParam :: String -> String -> MockRequest -> MockRequest
setBodyParam name value (MockRequest r) = r.setBodyParam name value

setRouteParam :: String -> String -> MockRequest -> MockRequest
setRouteParam name value (MockRequest r) = r.setRouteParam name value

setRequestCookie :: String -> String -> MockRequest -> MockRequest
setRequestCookie name value (MockRequest r) = r.setCookie name value

setRequestSignedCookie :: String -> String -> MockRequest -> MockRequest
setRequestSignedCookie name value (MockRequest r) = r.setSignedCookie name value

foreign import createMockApp ::
    forall e. Eff (express :: EXPRESS, err :: EXCEPTION, testOutput :: TESTOUTPUT | e) Application
foreign import createMockRequest ::
    forall e. String -> String -> ExpressM e MockRequest
foreign import sendMockRequest ::
    forall e. Application -> MockRequest -> ExpressM e MockResponse
foreign import sendMockError ::
    forall e. Application -> MockRequest -> String -> ExpressM e MockResponse

type TestExpress e = Aff (express :: EXPRESS, err :: EXCEPTION, testOutput :: TESTOUTPUT | e)
type TestMockApp e = ReaderT Application (TestExpress e) Unit
type TestApp e = App (err :: EXCEPTION, testOutput :: TESTOUTPUT | e)

testExpress :: forall e.
    String
    -> TestMockApp e
    -> TestSuite (express :: EXPRESS, err :: EXCEPTION, testOutput :: TESTOUTPUT | e)
testExpress testName assertions = test testName $ do
    mockApp <- liftEff $ createMockApp
    runReaderT assertions mockApp

setupMockApp :: forall e. TestApp e -> TestMockApp e
setupMockApp app = do
    mockApp <- ask
    liftEff $ apply app mockApp

sendRequest :: forall e.
    Method
    -> String
    -> (MockRequest -> MockRequest)
    -> (MockResponse -> TestMockApp e)
    -> TestMockApp e
sendRequest method url setupRequest testResponse = do
    app <- ask
    request <- liftEff $ map setupRequest $ createMockRequest (show method) url
    response <- liftEff $ sendMockRequest app request
    testResponse response

sendError :: forall e.
    Method
    -> String
    -> String
    -> (MockResponse -> TestMockApp e)
    -> TestMockApp e
sendError method url error testResponse = do
    app <- ask
    request <- liftEff $ createMockRequest (show method) url
    response <- liftEff $ sendMockError app request error
    testResponse response

assertMatch :: forall a e. (Show a, Eq a) => String -> a -> a -> Test e
assertMatch what expected actual = do
    let message = what <> " does not match: \
        \Expected [ " <> show expected <> " ], Got [ " <> show actual <> " ]"
    assert message (expected == actual)

type Reporter e = Test (express :: EXPRESS, testOutput :: TESTOUTPUT | e)
                  -> Eff (express :: EXPRESS, err :: EXCEPTION, testOutput :: TESTOUTPUT | e) Unit

assertInApp :: forall e. (Reporter e -> TestApp e) -> TestMockApp e
assertInApp assertion = do
    mockApp <- ask
    let reporter result = launchAff result >>= \_ -> pure unit
    liftEff $ apply (assertion reporter) mockApp

assertStatusCode :: forall e. Int -> MockResponse -> TestMockApp e
assertStatusCode expected response =
    lift $ assertMatch "Status code" expected response.statusCode

assertHeader :: forall e. String -> Maybe String -> MockResponse -> TestMockApp e
assertHeader name expected response = do
    let actual = StrMap.lookup name response.headers
    lift $ assertMatch ("Header '" <> name <> "'") expected actual

assertData :: forall e. String -> MockResponse -> TestMockApp e
assertData expected response =
    lift $ assertMatch "Response data" expected response.data

assertCookieValue :: forall e. String -> Maybe String -> MockResponse -> TestMockApp e
assertCookieValue name expected response = do
    let actual = map (\r -> r.value) $ StrMap.lookup name response.cookies
    lift $ assertMatch ("Cookie '" <> name <> "'") expected actual

setTestHeader :: forall e. String -> Handler e
setTestHeader = setResponseHeader "X-Test-Response-Header"

assertTestHeader :: forall e. Maybe String -> MockResponse -> TestMockApp e
assertTestHeader value response = assertHeader "X-Test-Response-Header" value response
