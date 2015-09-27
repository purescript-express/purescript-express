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
import Prelude hiding (id)
import Test.Mock
import Test.Unit
import Test.Unit.Console

testValue = "TestValue"
assertTestHeaderExists = assertTestHeader $ Just testValue
assertTestHeaderAbsent = assertTestHeader Nothing
assertTestHeaderWith = assertTestHeader <<< Just
sendTestRequest = sendRequest GET "http://example.com"

muteTest :: forall e. TestMockApp e
muteTest = lift $ assert "Muted" true

id :: forall a. a -> a
id a = a

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
    testExpress "Handler.getRequestHeader" $ do
        let testHeaderName = "X-Test-Header"
            setupRequest = setRequestHeader testHeaderName testValue
        setupMockApp $ use $
            getRequestHeader testHeaderName >>= maybe (return unit) setTestHeader
        sendTestRequest setupRequest assertTestHeaderExists

    testExpress "Handler.accepts" $ do
        let withAccepts = setRequestHeader "Accept"
        setupMockApp $ use $
            accepts "text/html" >>= maybe (return unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest (withAccepts "application/json") assertTestHeaderAbsent
        sendTestRequest (withAccepts "text/html") $ assertTestHeaderWith "text/html"
        sendTestRequest (withAccepts "text/xml, text/html") $ assertTestHeaderWith "text/html"

    testExpress "Handler.acceptsCharset" $ do
        let withAccepts = setRequestHeader "Accept-Charset"
        setupMockApp $ use $
            acceptsCharset "utf-8" >>= maybe (return unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest (withAccepts "cp-1251") assertTestHeaderAbsent
        sendTestRequest (withAccepts "utf-8") $ assertTestHeaderWith "utf-8"
        sendTestRequest (withAccepts "cp-1251, utf-8") $ assertTestHeaderWith "utf-8"

    testExpress "Handler.acceptsLanguage" $ do
        let withAccepts = setRequestHeader "Accept-Language"
        setupMockApp $ use $
            acceptsLanguage "en" >>= maybe (return unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest (withAccepts "ru") assertTestHeaderAbsent
        sendTestRequest (withAccepts "en") $ assertTestHeaderWith "en"
        sendTestRequest (withAccepts "ru, en, ch") $ assertTestHeaderWith "en"

    testExpress "Handler.hasType" $ do
        let contentType = "application/json"
            withContentType = setRequestHeader "Content-Type" contentType
        setupMockApp $ use $ do
            result <- hasType contentType
            if result then setTestHeader testValue else return unit
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest withContentType assertTestHeaderExists

testCookies = do
    testExpress "Handler.getCookie" $ do
        setupMockApp $ use $
            getCookie testCookie >>= maybe (return unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest withTestCookie assertTestHeaderExists
        sendTestRequest withTestSignedCookie assertTestHeaderAbsent
    testExpress "Handler.getSignedCookie" $ do
        setupMockApp $ use $
            getSignedCookie testCookie >>= maybe (return unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest withTestCookie assertTestHeaderAbsent
        sendTestRequest withTestSignedCookie assertTestHeaderExists
  where
    testCookie = "cookie"
    withTestCookie = setRequestCookie testCookie testValue
    withTestSignedCookie = setRequestSignedCookie testCookie testValue

testMisc = do
    testExpress "Handler.getRoute" $ do
        let route = "/some/(.+)/path"
        setupMockApp $ get route $ getRoute >>= setTestHeader
        sendRequest GET "http://example.com/" id assertTestHeaderAbsent
        sendRequest GET "http://example.com/some/possible/path" id $ assertTestHeaderWith route
        sendRequest GET "http://example.com/some/another/path" id $ assertTestHeaderWith route
        sendRequest GET "http://example.com/some/path" id assertTestHeaderAbsent

    testExpress "Handler.getRemoteIp" $ do
        setupMockApp $ use $ getRemoteIp >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "0.0.0.0"

    testExpress "Handler.getRemoteIps" $ do
        let ips = ["0.0.0.0", "0.0.0.1", "0.0.0.2"]
        setupMockApp $ use $ getRemoteIps >>= (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith (show ips)

    testExpress "Handler.getPath" $ do
        setupMockApp $ use $ getPath >>= setTestHeader
        sendRequest GET "http://example.com/" id $ assertTestHeaderWith "/"
        sendRequest GET "http://example.com/some/path" id $ assertTestHeaderWith "/some/path"

    testExpress "Handler.getHostname" $ do
        setupMockApp $ use $ getHostname >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "example.com"

    testExpress "Handler.getSubdomains" $ do
        setupMockApp $ use $ getSubdomains >>= (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith "[]"
        sendRequest GET "http://foo.bar.baz.com" id $ assertTestHeaderWith $ show ["foo", "bar"]

    testExpress "Handler.isFresh" $ do
        setupMockApp $ use $ do
            result <- (\f s -> f && not s) <$> isFresh <*> isStale
            if result then setTestHeader testValue else return unit
        sendTestRequest id assertTestHeaderExists

    testExpress "Handler.isXhr" $ do
        setupMockApp $ use $ do
            result <- isXhr
            if not result then setTestHeader testValue else return unit
        sendTestRequest id assertTestHeaderExists

    testExpress "Handler.getProtocol" $ do
        setupMockApp $ use $ getProtocol >>= maybe (return unit) (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith (show Http)

    testExpress "Handler.getMethod" $ do
        setupMockApp $ use $ getMethod >>= maybe (return unit) (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith (show GET)

    testExpress "Handler.getUrl" $ do
        setupMockApp $ use $ getUrl >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "http://example.com"

    testExpress "Handler.getOriginalUrl" $ do
        setupMockApp $ use $ getOriginalUrl >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "http://example.com"

testSuite = do
    testParams
    testHeaders
    testCookies
    testMisc
