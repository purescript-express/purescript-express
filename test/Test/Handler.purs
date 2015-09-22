module Test.Handler (testSuite) where

import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Trans
import Data.Array (head)
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

testValue = "TestValue"
assertTestHeaderExists = assertTestHeader $ Just testValue
assertTestHeaderAbsent = assertTestHeader Nothing
sendTestRequest = sendRequest GET "http://example.com"

muteTest :: forall e. TestMockApp e
muteTest = lift $ assert "Muted" true

testParams = do
    testExpress "Handler.getRouteParam" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendTestRequest withRouteParam assertTestHeaderExists
    testExpress "Handler.getBodyParam" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendTestRequest withBodyParam assertTestHeaderExists
    testExpress "Handler.getQueryParam" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendRequest GET urlWithQueryParam id assertTestHeaderExists
    testExpress "Handler.getQueryParams" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendRequest GET urlWithQueryParams id assertTestHeaderExists
  where
    id a = a
    testParam = "param"
    withoutParams  = id
    withRouteParam = setRouteParam testParam testValue
    withBodyParam  = setBodyParam  testParam testValue
    urlWithQueryParam = "http://example.com?" ++ testParam ++ "=" ++ testValue
    urlWithQueryParams = urlWithQueryParam ++ "&" ++ testParam ++ "=someOtherValue"
    paramsHandler  = do
        getRouteParam testParam >>= maybe (return unit) setTestHeader
        getBodyParam  testParam >>= maybe (return unit) setTestHeader
        getQueryParam testParam >>= maybe (return unit) setTestHeader
        map head (getQueryParams testParam) >>= maybe (return unit) setTestHeader

testHeaders = do
    testExpress "Handler.getRequestHeader" muteTest
    testExpress "Handler.accepts" muteTest
    testExpress "Handler.acceptsCharset" muteTest
    testExpress "Handler.acceptsLanguage" muteTest
    testExpress "Handler.hasType" muteTest

testCookies = do
    testExpress "Handler.getCookie" muteTest
    testExpress "Handler.getSignedCookie" muteTest

testMisc = do
    testExpress "Handler.getRoute" muteTest
    testExpress "Handler.getRemoteIp" muteTest
    testExpress "Handler.getRemoteIps" muteTest
    testExpress "Handler.getPath" muteTest
    testExpress "Handler.getHostname" muteTest
    testExpress "Handler.getSubdomains" muteTest
    testExpress "Handler.isFresh" muteTest
    testExpress "Handler.isStale" muteTest
    testExpress "Handler.isXhr" muteTest
    testExpress "Handler.getProtocol" muteTest
    testExpress "Handler.getMethod" muteTest
    testExpress "Handler.getUrl" muteTest
    testExpress "Handler.getOriginalUrl" muteTest

testSuite = do
    testParams
    testHeaders
    testCookies
    testMisc
