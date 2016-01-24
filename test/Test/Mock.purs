module Test.Mock
    ( MockResponse(..)
    , MockRequest(..)
    , MockCookie(..)
    , setRequestHeader
    , setRouteParam
    , setBodyParam
    , setRequestCookie
    , setRequestSignedCookie
    , TestUnitM(..)
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
import Data.Function
import Data.Maybe
import qualified Data.StrMap as StrMap
import Data.Tuple
import Node.Express.App
import Node.Express.Handler
import Node.Express.Types
import Node.Express.Response
import Prelude hiding (apply)
import Test.Unit
import Test.Unit.Console

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
    , setBodyParam :: String -> String -> MockRequest
    , setRouteParam :: String -> String -> MockRequest
    , setCookie :: String -> String -> MockRequest
    , setSignedCookie :: String -> String -> MockRequest
    }

setRequestHeader :: String -> String -> MockRequest -> MockRequest
setRequestHeader name value (MockRequest r) = r.setHeader name value

setBodyParam :: String -> String -> MockRequest -> MockRequest
setBodyParam name value (MockRequest r) = r.setBodyParam name value

setRouteParam :: String -> String -> MockRequest -> MockRequest
setRouteParam name value (MockRequest r) = r.setRouteParam name value

setRequestCookie :: String -> String -> MockRequest -> MockRequest
setRequestCookie name value (MockRequest r) = r.setCookie name value

setRequestSignedCookie :: String -> String -> MockRequest -> MockRequest
setRequestSignedCookie name value (MockRequest r) = r.setSignedCookie name value

foreign import createMockApp ::
    forall e. Eff e Application
foreign import createMockRequest ::
    forall e. String -> String -> ExpressM e MockRequest
foreign import sendMockRequest ::
    forall e. Application -> MockRequest -> ExpressM e MockResponse
foreign import sendMockError ::
    forall e. Application -> MockRequest -> String -> ExpressM e MockResponse

type TestUnitM e = ExceptT String (ContT Unit (Eff e))
type TestMockApp e = ReaderT Application (TestUnitM e) Unit

testExpress :: forall e.
    String
    -> TestMockApp (express :: EXPRESS, testOutput :: TestOutput | e)
    -> Assertion (express :: EXPRESS, testOutput :: TestOutput | e)
testExpress testName assertions = test testName $ do
    mockApp <- lift $ lift $ createMockApp
    runReaderT assertions mockApp

setupMockApp :: forall e. App e -> TestMockApp (express :: EXPRESS | e)
setupMockApp app = do
    mockApp <- ask
    liftEff $ apply app mockApp

sendRequest :: forall e.
    Method
    -> String
    -> (MockRequest -> MockRequest)
    -> (MockResponse -> TestMockApp (express :: EXPRESS | e))
    -> TestMockApp (express :: EXPRESS | e)
sendRequest method url setupRequest testResponse = do
    app <- ask
    request <- liftEff $ map setupRequest $ createMockRequest (show method) url
    response <- liftEff $ sendMockRequest app request
    testResponse response

sendError :: forall e.
    Method
    -> String
    -> String
    -> (MockResponse -> TestMockApp (express :: EXPRESS | e))
    -> TestMockApp (express :: EXPRESS | e)
sendError method url error testResponse = do
    app <- ask
    request <- liftEff $ createMockRequest (show method) url
    response <- liftEff $ sendMockError app request error
    testResponse response

assertMatch :: forall a e. (Show a, Eq a) => String -> a -> a -> Assertion e
assertMatch what expected actual = do
    let message = what ++ " does not match: \
        \Expected [ " ++ show expected ++ " ], Got [ " ++ show actual ++ " ]"
    assert message (expected == actual)

assertInApp :: forall e.
    ((TestResult -> Eff (express :: EXPRESS | e) Unit) -> App e)
    -> TestMockApp (express :: EXPRESS | e)
assertInApp assertion = do
    mockApp <- ask
    let tester callback = liftEff $ apply (assertion callback) mockApp
    lift $ testFn tester

assertStatusCode :: forall e. Int -> MockResponse -> TestMockApp e
assertStatusCode expected response =
    lift $ assertMatch "Status code" expected response.statusCode

assertHeader :: forall e. String -> Maybe String -> MockResponse -> TestMockApp e
assertHeader name expected response = do
    let actual = StrMap.lookup name response.headers
    lift $ assertMatch ("Header '" ++ name ++ "'") expected actual

assertData :: forall e. String -> MockResponse -> TestMockApp e
assertData expected response =
    lift $ assertMatch "Response data" expected response.data

assertCookieValue :: forall e. String -> Maybe String -> MockResponse -> TestMockApp e
assertCookieValue name expected response = do
    let actual = map (\r -> r.value) $ StrMap.lookup name response.cookies
    lift $ assertMatch ("Cookie '" ++ name ++ "'") expected actual

setTestHeader :: forall e. String -> Handler e
setTestHeader = setResponseHeader "X-Test-Response-Header"

assertTestHeader :: forall e. Maybe String -> MockResponse -> TestMockApp e
assertTestHeader value response = assertHeader "X-Test-Response-Header" value response
