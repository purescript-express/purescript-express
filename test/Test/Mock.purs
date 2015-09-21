module Test.Mock
    ( MockResponse(..)
    , MockRequest(..)
    , TestUnitM(..)
    , TestMockApp(..)
    , createMockApp
    , createMockRequest
    , testExpress
    , setupMockApp
    , assertInApp
    , sendRequest
    , sendError
    , assertMatch
    , assertHeader
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
import Prelude hiding (apply)
import Test.Unit
import Test.Unit.Console

type MockResponse = {
    status      :: Int,
    contentType :: String,
    headers     :: StrMap.StrMap String,
    data        :: String
}

type MockRequest = {
    setHeader :: forall e. String -> String -> Eff (express :: Express | e) Unit,
    setBodyParam :: forall e. String -> String -> Eff (express :: Express | e) Unit,
    setRouteParam :: forall e. String -> String -> Eff (express :: Express | e) Unit
}

foreign import createMockApp ::
    forall e. Eff e Application
foreign import createMockRequest ::
    forall e. String -> String -> ExpressM MockRequest
foreign import sendMockRequest ::
    forall e. Application -> MockRequest -> ExpressM MockResponse
foreign import sendMockError ::
    forall e. Application -> MockRequest -> String -> ExpressM MockResponse

type TestUnitM e = ExceptT String (ContT Unit (Eff e))
type TestMockApp e = ReaderT Application (TestUnitM e) Unit

testExpress :: forall e.
    String
    -> TestMockApp (express :: Express, testOutput :: TestOutput | e)
    -> Assertion (express :: Express, testOutput :: TestOutput | e)
testExpress testName assertions = test testName $ do
    mockApp <- lift $ lift $ createMockApp
    runReaderT assertions mockApp

setupMockApp :: forall e. App -> TestMockApp (express :: Express | e)
setupMockApp app = do
    mockApp <- ask
    liftEff $ apply app mockApp

assertInApp :: forall e.
    ((TestResult -> Eff (express :: Express | e) Unit) -> App)
    -> TestMockApp (express :: Express | e)
assertInApp assertion = do
    mockApp <- ask
    let tester callback = liftEff $ apply (assertion callback) mockApp
    lift $ testFn tester

sendRequest :: forall e.
    MockRequest
    -> (MockResponse -> TestMockApp (express :: Express | e))
    -> TestMockApp (express :: Express | e)
sendRequest request testResponse = do
    app <- ask
    response <- liftEff $ sendMockRequest app request
    testResponse response

sendError :: forall e.
    MockRequest
    -> String
    -> (MockResponse -> TestMockApp (express :: Express | e))
    -> TestMockApp (express :: Express | e)
sendError request error testResponse = do
    app <- ask
    response <- liftEff $ sendMockError app request error
    testResponse response

assertMatch :: forall a e. (Show a, Eq a) => String -> Maybe a -> Maybe a -> Assertion e
assertMatch what expected actual = do
    let message = what ++ " does not match: \
        \Expected [ " ++ show expected ++ " ], Got [ " ++ show actual ++ " ]"
    assert message (expected == actual)

assertHeader :: forall e. MockResponse -> String -> Maybe String -> TestMockApp e
assertHeader response name expected = do
    let actual = StrMap.lookup name response.headers
    lift $ assertMatch ("Header '" ++ name ++ "'") expected actual

