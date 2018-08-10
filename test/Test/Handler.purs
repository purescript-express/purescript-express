module Test.Handler (testSuite) where

import Effect
import Effect.Class
import Effect.Exception
import Control.Monad.Trans.Class
import Data.Function
import Data.Maybe
import Node.Express.App hiding (apply)
import Node.Express.Handler
import Node.Express.Request
import Node.Express.Response
import Node.Express.Test.Mock
import Node.Express.Types
import Prelude
import Test.Unit
import Test.Unit.Assert
import Test.Unit.Console
import Unsafe.Coerce

import Control.Monad.Except (runExcept)
import Data.Array (head, null)
import Data.Either (either)
import Foreign (Foreign, unsafeToForeign, readString)
import Foreign.Class (encode, decode)
import Foreign.Object (Object)
import Global.Unsafe (unsafeStringify)


foreign import cwdJson :: String
foreign import unsafeUpdateMapInPlace :: forall a. Object a -> String -> a -> Effect Unit

id :: forall a. a -> a
id a = a

testValue = "TestValue"
assertTestHeaderExists = assertTestHeader $ Just testValue
assertTestHeaderAbsent = assertTestHeader Nothing
assertTestHeaderWith = assertTestHeader <<< Just
sendTestRequest = sendRequest GET "http://example.com"

muteTest :: TestMockApp
muteTest = lift $ assert "Muted" true

testParams = do
    testExpress "getRouteParam" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendTestRequest withRouteParam assertTestHeaderExists
    testExpress "getBody" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendTestRequest withBody assertTestHeaderExists
    testExpress "getBody'" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendTestRequest withBody assertTestHeaderExists
        sendTestRequest withBody' assertTestHeaderExists
    testExpress "getBodyParam" $ do
        setupMockApp $ use paramsHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
        sendTestRequest withBodyParam assertTestHeaderExists
    testExpress "getQueryParam (without params)" $ do
        setupMockApp $ use $ queryParamHandler
        sendTestRequest withoutParams assertTestHeaderAbsent
    testExpress "getQueryParam (string)" $ do
        setupMockApp $ use $ queryParamHandler
        let url = "http://example.com/?param=testValue"
        sendRequest GET url id $ assertTestHeaderWith "testValue"
    testExpress "getQueryParam (array)" $ do
        setupMockApp $ use $ queryParamArrayHandler
        let url = "http://example.com/?param=1&param=2"
        sendRequest GET url id $ assertTestHeaderWith $ show ["1", "2"]
  where
    testParam = "param"
    withoutParams  = id
    withRouteParam = setRouteParam testParam testValue
    withBody       = setBody       testValue
    withBody'      = setBody' $ encode testValue
    withBodyParam  = setBodyParam  testParam testValue
    getBody'_ = getBody' <#> decode
    paramsHandler = do
        getRouteParam testParam >>= maybe (pure unit) setTestHeader
        getBody >>= either (pure <<< const unit) setTestHeader <<< runExcept
        getBody'_ >>= either (pure <<< const unit) setTestHeader <<< runExcept
        getBodyParam  testParam >>= maybe (pure unit) setTestHeader
    queryParamHandler =
        getQueryParam testParam >>= maybe (pure unit) setTestHeader
    queryParamArrayHandler =
        getQueryParams testParam >>= maybe (pure unit) (\val -> setTestHeader $ show (val :: Array String))

testHeaders = do
    testExpress "getRequestHeader" $ do
        let testHeaderName = "X-Test-Header"
            setupRequest = setRequestHeader testHeaderName testValue
        setupMockApp $ use $
            getRequestHeader testHeaderName >>= maybe (pure unit) setTestHeader
        sendTestRequest setupRequest assertTestHeaderExists

    testExpress "accepts" $ do
        let withAccepts = setRequestHeader "Accept"
        setupMockApp $ use $
            accepts "text/html" >>= maybe (pure unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest (withAccepts "application/json") assertTestHeaderAbsent
        sendTestRequest (withAccepts "text/html") $ assertTestHeaderWith "text/html"
        sendTestRequest (withAccepts "text/xml, text/html") $ assertTestHeaderWith "text/html"

    testExpress "acceptsCharset" $ do
        let withAccepts = setRequestHeader "Accept-Charset"
        setupMockApp $ use $
            acceptsCharset "utf-8" >>= maybe (pure unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest (withAccepts "cp-1251") assertTestHeaderAbsent
        sendTestRequest (withAccepts "utf-8") $ assertTestHeaderWith "utf-8"
        sendTestRequest (withAccepts "cp-1251, utf-8") $ assertTestHeaderWith "utf-8"

    testExpress "acceptsLanguage" $ do
        let withAccepts = setRequestHeader "Accept-Language"
        setupMockApp $ use $
            acceptsLanguage "en" >>= maybe (pure unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest (withAccepts "ru") assertTestHeaderAbsent
        sendTestRequest (withAccepts "en") $ assertTestHeaderWith "en"
        sendTestRequest (withAccepts "ru, en, ch") $ assertTestHeaderWith "en"

    testExpress "hasType" $ do
        let contentType = "application/json"
            withContentType = setRequestHeader "Content-Type" contentType
        setupMockApp $ use $ do
            result <- hasType contentType
            if result then setTestHeader testValue else pure unit
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest withContentType assertTestHeaderExists

testCookies = do
    testExpress "getCookie" $ do
        setupMockApp $ use $
            getCookie testCookie >>= maybe (pure unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest withTestCookie assertTestHeaderExists
        sendTestRequest withTestSignedCookie assertTestHeaderAbsent
    testExpress "getSignedCookie" $ do
        setupMockApp $ use $
            getSignedCookie testCookie >>= maybe (pure unit) setTestHeader
        sendTestRequest id assertTestHeaderAbsent
        sendTestRequest withTestCookie assertTestHeaderAbsent
        sendTestRequest withTestSignedCookie assertTestHeaderExists
  where
    testCookie = "cookie"
    withTestCookie = setRequestCookie testCookie testValue
    withTestSignedCookie = setRequestSignedCookie testCookie testValue

testMisc = do
    testExpress "getRoute" $ do
        let route = "/some/(.+)/path"
        setupMockApp $ get route $ getRoute >>= setTestHeader
        sendRequest GET "http://example.com/" id assertTestHeaderAbsent
        sendRequest GET "http://example.com/some/possible/path" id $ assertTestHeaderWith route
        sendRequest GET "http://example.com/some/another/path" id $ assertTestHeaderWith route
        sendRequest GET "http://example.com/some/path" id assertTestHeaderAbsent

    testExpress "getRemoteIp" $ do
        setupMockApp $ use $ getRemoteIp >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "0.0.0.0"

    testExpress "getRemoteIps" $ do
        let ips = ["0.0.0.0", "0.0.0.1", "0.0.0.2"]
        setupMockApp $ use $ getRemoteIps >>= (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith (show ips)

    testExpress "getPath" $ do
        setupMockApp $ use $ getPath >>= setTestHeader
        sendRequest GET "http://example.com/" id $ assertTestHeaderWith "/"
        sendRequest GET "http://example.com/some/path" id $ assertTestHeaderWith "/some/path"

    testExpress "getHostname" $ do
        setupMockApp $ use $ getHostname >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "example.com"

    testExpress "getSubdomains" $ do
        setupMockApp $ use $ getSubdomains >>= (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith "[]"
        sendRequest GET "http://foo.bar.baz.com" id $ assertTestHeaderWith $ show ["foo", "bar"]

    testExpress "isFresh" $ do
        setupMockApp $ use $ do
            result <- (\f s -> f && not s) <$> isFresh <*> isStale
            if result then setTestHeader testValue else pure unit
        sendTestRequest id assertTestHeaderExists

    testExpress "isXhr" $ do
        setupMockApp $ use $ do
            result <- isXhr
            if not result then setTestHeader testValue else pure unit
        sendTestRequest id assertTestHeaderExists

    testExpress "getProtocol" $ do
        setupMockApp $ use $ getProtocol >>= maybe (pure unit) (show >>> setTestHeader)
        sendTestRequest id $ assertTestHeaderWith (show Http)

    testExpress "getMethod" $ do
        setupMockApp $ use $ getMethod >>= show >>> setTestHeader
        sendTestRequest id $ assertTestHeaderWith (show GET)

    testExpress "getUrl" $ do
        setupMockApp $ use $ getUrl >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "http://example.com"

    testExpress "getOriginalUrl" $ do
        setupMockApp $ use $ getOriginalUrl >>= setTestHeader
        sendTestRequest id $ assertTestHeaderWith "http://example.com"

    testExpress "getUserData + setUserData" $ do
        setupMockApp $ do
            use $ setUserData "key" "TEST-DATA"
            get "/" $ getUserData "key" >>= maybe (pure unit) setTestHeader
        sendTestRequest id $ assertTestHeaderWith "TEST-DATA"

testResponse = do
    testExpress "setStatus" $ do
        sendTestRequest id $ assertStatusCode 0
        setupMockApp $ use $ setStatus 200
        sendTestRequest id $ assertStatusCode 200

    testExpress "(get)setResponseHeader" $ do
        setupMockApp $ use $ do
            setResponseHeader "X-Foo-Bar" "foo"
            maybeFoo <- getResponseHeader "X-Foo-Bar"
            if maybeFoo == Just "foo"
                then setTestHeader testValue
                else pure unit
        sendTestRequest id assertTestHeaderExists

    testExpress "headersSent" $ do
        setupMockApp $ use $ do
            headersAreNotSentBeforeSend <- map not headersSent
            send "Something"
            headersAreSentAfterSend <- headersSent
            if (headersAreNotSentBeforeSend && headersAreSentAfterSend)
                then setTestHeader testValue
                else pure unit
        sendTestRequest id assertTestHeaderExists

    testExpress "setCookie" $ do
        setupMockApp $ use $ setCookie testCookie testValue defaultCookieOptions
        sendTestRequest id (assertCookieValue testCookie $ Just testValue)

    testExpress "clearCookie" $ do
        let withTestCookie = setRequestCookie testCookie testValue
            assertTestCookieAbsent = assertCookieValue testCookie Nothing
        setupMockApp $ use $ clearCookie testCookie "/"
        sendTestRequest id assertTestCookieAbsent
        sendTestRequest withTestCookie assertTestCookieAbsent

    testExpress "send" $ do
        setupMockApp $ use $ send testValue
        sendTestRequest id $ assertData testValue

    testExpress "end" $ do
        setupMockApp $ use end
        sendTestRequest id $ \response -> do
            assertStatusCode 200 response

    testExpress "sendJson" $ do
        setupMockApp $ use $ sendJson testData
        sendTestRequest id $ assertData testDataStr

    testExpress "sendJsonp" $ do
        setupMockApp $ use $ sendJsonp testData
        sendTestRequest id $ assertData testDataStr

    testExpress "render" $ do
        setupMockApp $ use $ render "test-view" testData
        sendTestRequest id $ assertData testDataRendered

    testExpress "redirect" $ do
        setupMockApp $ use $ redirect exampleCom
        sendTestRequest id $ \response -> do
            assertStatusCode 302 response
            assertHeader "Location" (Just exampleCom) response

    testExpress "redirectWithStatus" $ do
        setupMockApp $ use $ redirectWithStatus 301 exampleCom
        sendTestRequest id $ \response -> do
            assertStatusCode 301 response
            assertHeader "Location" (Just exampleCom) response

    testExpress "setLocation" $ do
        setupMockApp $ use $ setLocation exampleCom
        sendTestRequest id $ assertHeader "Location" (Just exampleCom)

    testExpress "setContentType" $ do
        setupMockApp $ use $ setContentType "text/html"
        sendTestRequest id $ assertHeader "Content-Type" (Just "text/html")

    testExpress "sendFile" $ do
        setupMockApp $ use $ sendFile testFile
        sendTestRequest id $ \response -> do
            assertHeader filepathHeader (Just testFile) response
            assertData ("{\"root\":" <> cwdJson <> "}") response

    testExpress "sendFileExt" $ do
        setupMockApp $ use $ sendFileExt testFile testData (\_ -> pure unit)
        sendTestRequest id $ \response -> do
            assertHeader filepathHeader (Just testFile) response
            assertData testDataStr response

    testExpress "sendFileExt (with error)" $ do
        setupMockApp $ use $ sendFileExt testFile {triggerError: true} testErrorHandler
        sendTestRequest id $ assertHeader testErrorHeader (Just testValue)

    testExpress "download" $ do
        setupMockApp $ use $ download testFile
        sendTestRequest id $ \response -> do
            assertHeader filepathHeader (Just testFile) response
            assertHeader realFilepathHeader (Just testFile) response

    testExpress "downloadExt" $ do
        setupMockApp $ use $ downloadExt testFile "renamed.txt" (\_ -> pure unit)
        sendTestRequest id $ \response -> do
            assertHeader filepathHeader (Just "renamed.txt") response
            assertHeader realFilepathHeader (Just testFile) response

    testExpress "downloadExt (with error)" $ do
        setupMockApp $ use $ downloadExt testFile "triggerError" testErrorHandler
        sendTestRequest id $ assertHeader testErrorHeader (Just testValue)
  where
    exampleCom = "http://example.com"
    testCookie = "testCookie"
    testData = {foo: "bar"}
    testDataStr = "{\"foo\":\"bar\"}"
    testDataRendered = "Rendered test-view with data: {\"foo\":\"bar\"}"
    testFile = "test.txt"
    filepathHeader = "X-Filepath"
    realFilepathHeader = "X-Real-Filepath"
    testErrorHeader = "X-Test-Error"
    testErrorHandler :: Error -> Effect Unit
    testErrorHandler = \respAsError ->
        let response = unsafeCoerce respAsError in
        unsafeUpdateMapInPlace (response.headers) testErrorHeader testValue

testSuite = suite "Handler" do
    testParams
    testHeaders
    testCookies
    testMisc
    testResponse
