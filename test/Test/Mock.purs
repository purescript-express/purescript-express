module Test.Mock
    ( MockResponse(..)
    , MockRequest(..)
    , TestAppM(..)
    , TestApp(..)
    , createMockApp
    , createMockRequest
    , testApp
    , report
    , getMockApp
    , sendRequest
    , sendError
    , assertMatch
    , assertHeader
    ) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
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
    setHeader :: forall e. String -> String -> Eff e Unit,
    setBodyParam :: forall e. String -> String -> Eff e Unit,
    setRouteParam :: forall e. String -> String -> Eff e Unit
}

foreign import createMockApp ::
    forall e. Eff e Application
foreign import createMockRequest ::
    forall e. String -> String -> Eff e MockRequest
foreign import sendMockRequest ::
    forall e. Application -> MockRequest -> Eff e MockResponse
foreign import sendMockError ::
    forall e. Application -> MockRequest -> String -> Eff e MockResponse

type TestAppM = ReaderT (Tuple Application (TestResult -> App)) AppM
type TestApp = TestAppM Unit

testApp :: forall e.
           String -> TestApp ->
           Assertion ( express :: Express, testOutput :: TestOutput | e)
testApp testName appGenerator = test testName $ testFn tester where
    tester callback = do
        mockApp <- createMockApp
        let app = runReaderT appGenerator $ Tuple mockApp (liftEff <<< callback)
        liftEff $ apply app mockApp

-- TODO: report type inference issue
appAndReporter :: TestAppM (Tuple Application (TestResult -> App))
appAndReporter = ask

report :: TestResult -> TestApp
report message = do
    reporter <- map snd appAndReporter
    lift $ reporter message

getMockApp :: TestAppM Application
getMockApp = map fst appAndReporter

sendRequest :: MockRequest -> TestAppM MockResponse
sendRequest request =
    getMockApp >>= \app -> liftEff $ sendMockRequest app request

sendError :: MockRequest -> String -> TestAppM MockResponse
sendError request error =
    getMockApp >>= \app -> liftEff $ sendMockError app request error

assertMatch :: forall a. (Show a, Eq a) => String -> Maybe a -> Maybe a -> TestApp
assertMatch what expected actual = do
    let message = what ++ " does not match: \
        \Expected [ " ++ show expected ++ " ], Got [ " ++ show actual ++ " ]"
    if expected == actual
        then report success
        else report $ failure message

assertHeader :: MockResponse -> String -> Maybe String -> TestApp
assertHeader response name expected = do
    let actual = StrMap.lookup name response.headers
    assertMatch ("Header '" ++ name ++ "'") expected actual

