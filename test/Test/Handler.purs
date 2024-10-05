module Test.Handler (testSuite) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free)
import Control.Monad.Reader (ReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throwException)
import Foreign.Generic.Class (encode, decode)
import Foreign.Object (Object)
import Node.Express.App (get, use)
import Node.Express.Request (accepts, acceptsCharset, acceptsLanguage, getBody, getBody', getBodyParam, getCookie, getHostname, getMethod, getOriginalUrl, getPath, getProtocol, getQueryParam, getQueryParams, getRemoteIp, getRemoteIps, getRequestHeader, getRoute, getRouteParam, getSignedCookie, getSubdomains, getUrl, getUserData, hasType, isFresh, isStale, isXhr, setUserData)
import Node.Express.Response (clearCookie, defaultDownloadOptions, defaultSendFileOptions, download, downloadExt, end, getResponseHeader, headersSent, redirect, redirectWithStatus, render, send, sendFile, sendFileExt, sendJson, sendJsonp, setContentType, setCookie, setLocation, setResponseHeader, setStatus)
import Node.Express.Test.Mock (MockRequest, assertCookieValue, assertData, assertHeader, assertStatusCode, assertTestHeader, sendRequest, setBody, setBody', setBodyParam, setRequestCookie, setRequestHeader, setRequestSignedCookie, setRouteParam, setTestHeader, setupMockApp, testExpress)
import Node.Express.Types (Application, DownloadFileName(..), Method(..), Protocol(..), Status(..), defaultCookieOptions)
import Test.Unit (TestF, suite)

foreign import cwdJson :: String
foreign import unsafeUpdateMapInPlace :: forall a. Object a -> String -> a -> Effect Unit
foreign import unsafeStringify :: forall a. a -> String

id :: forall a. a -> a
id a = a

testValue :: String
testValue = "TestValue"

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

assertTestHeaderWith
  :: String
  -> { cookies ::
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
assertTestHeaderWith = assertTestHeader <<< Just

sendTestRequest
  :: (MockRequest -> MockRequest)
  -> ( { cookies ::
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
     )
  -> ReaderT Application Aff Unit
sendTestRequest = sendRequest GET "http://example.com"

-- muteTest :: TestMockApp
-- muteTest = lift $ assert "Muted" true

testParams :: Free TestF Unit
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
    sendRequest GET url id $ assertTestHeaderWith $ show [ "1", "2" ]
  where
  testParam = "param"
  withoutParams = id
  withRouteParam = setRouteParam testParam testValue
  withBody = setBody testValue
  withBody' = setBody' $ encode testValue
  withBodyParam = setBodyParam testParam testValue
  getBody'_ = getBody' <#> decode
  paramsHandler = do
    getRouteParam testParam >>= maybe (pure unit) setTestHeader
    getBody >>= either (pure <<< const unit) setTestHeader <<< runExcept
    getBody'_ >>= either (pure <<< const unit) setTestHeader <<< runExcept
    getBodyParam testParam >>= maybe (pure unit) setTestHeader
  queryParamHandler =
    getQueryParam testParam >>= maybe (pure unit) setTestHeader
  queryParamArrayHandler =
    getQueryParams testParam >>= maybe (pure unit) (\val -> setTestHeader $ show (val :: Array String))

testHeaders :: Free TestF Unit
testHeaders = do
  testExpress "getRequestHeader" $ do
    let
      testHeaderName = "X-Test-Header"
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
    let
      contentType = "application/json"
      withContentType = setRequestHeader "Content-Type" contentType
    setupMockApp $ use $ do
      result <- hasType contentType
      if result then setTestHeader testValue else pure unit
    sendTestRequest id assertTestHeaderAbsent
    sendTestRequest withContentType assertTestHeaderExists

testCookies :: Free TestF Unit
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

testMisc :: Free TestF Unit
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
    let ips = [ "0.0.0.0", "0.0.0.1", "0.0.0.2" ]
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
    sendRequest GET "http://foo.bar.baz.com" id $ assertTestHeaderWith $ show [ "foo", "bar" ]

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

testResponse :: Free TestF Unit
testResponse = do
  testExpress "setStatus" $ do
    sendTestRequest id $ assertStatusCode 0
    setupMockApp $ use $ setStatus 200
    sendTestRequest id $ assertStatusCode 200

  testExpress "(get)setResponseHeader" $ do
    setupMockApp $ use $ do
      setResponseHeader "X-Foo-Bar" "foo"
      maybeFoo <- getResponseHeader "X-Foo-Bar"
      if maybeFoo == Just "foo" then setTestHeader testValue
      else pure unit
    sendTestRequest id assertTestHeaderExists

  testExpress "headersSent" $ do
    setupMockApp $ use $ do
      headersAreNotSentBeforeSend <- map not headersSent
      send "Something"
      headersAreSentAfterSend <- headersSent
      if (headersAreNotSentBeforeSend && headersAreSentAfterSend) then setTestHeader testValue
      else pure unit
    sendTestRequest id assertTestHeaderExists

  testExpress "setCookie" $ do
    setupMockApp $ use $ setCookie testCookie testValue defaultCookieOptions
    sendTestRequest id (assertCookieValue testCookie $ Just testValue)

  testExpress "clearCookie" $ do
    let
      withTestCookie = setRequestCookie testCookie testValue
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
    setupMockApp $ use $ redirectWithStatus (Status 301) exampleCom
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
    setupMockApp $ use $ sendFile testFile (\error -> throwException error)
    sendTestRequest id $ \response -> do
      assertHeader filepathHeader (Just testFile) response
  -- assertData ("{\"root\":" <> cwdJson <> "}") response

  testExpress "sendFileExt" $ do
    setupMockApp $ use $ sendFileExt testFile defaultSendFileOptions throwException
    sendTestRequest id $ \response -> do
      assertHeader filepathHeader (Just testFile) response
  -- assertData testDataStr response -- TODO?

  -- testExpress "sendFileExt (with error)" $ do
  --   setupMockApp $ use $ sendFileExt testFile defaultSendFileOptions testErrorHandler
  --   sendTestRequest id $ assertHeader testErrorHeader (Just testValue)

  testExpress "download" $ do
    setupMockApp $ use $ download testFile defaultDownloadOptions throwException
    sendTestRequest id $ \response -> do
      assertHeader filepathHeader (Just testFile) response
      assertHeader realFilepathHeader (Just testFile) response

  testExpress "downloadExt" $ do
    setupMockApp $ use $ downloadExt testFile (DownloadFileName "renamed.txt") defaultDownloadOptions (\_ -> pure unit)
    sendTestRequest id $ \response -> do
      assertHeader filepathHeader (Just "renamed.txt") response
      assertHeader realFilepathHeader (Just testFile) response

  -- testExpress "downloadExt (with error)" $ do
  --   let options = defaultDownloadOptions
  --         { headers = Object.fromFoldable [Tuple "X-Test-Error" "adf"]
  --         }
  --   traceM options
  --   setupMockApp $ use $ downloadExt testFile (DownloadFileName "triggerError") options testErrorHandler
  --   sendTestRequest id $ \response -> do
  --     traceM "RESPONSE"
  --     traceM response
  --     assertHeader testErrorHeader (Just testValue) response
  where
  exampleCom = "http://example.com"
  testCookie = "testCookie"
  testData = { foo: "bar" }
  testDataStr = "{\"foo\":\"bar\"}"
  testDataRendered = "Rendered test-view with data: {\"foo\":\"bar\"}"
  testFile = "test.txt"
  filepathHeader = "X-Filepath"
  realFilepathHeader = "X-Real-Filepath"

testSuite :: Free TestF Unit
testSuite = suite "Handler" do
  testParams
  testHeaders
  testCookies
  testMisc
  testResponse
