module Node.Express.Handler
    ( HandlerM()
    , Handler()
    , withHandler, capture, next, nextThrow
    -- Request
    , getRouteParam, getQueryParam, getQueryParams, getBodyParam
    , getRoute
    , getCookie, getSignedCookie
    , getRequestHeader
    , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
    , getRemoteIp, getRemoteIps, getPath, getHostname, getSubdomains
    , isFresh, isStale
    , isXhr, getProtocol, getMethod
    , getUrl, getOriginalUrl
    -- Response
    , setStatus
    , getResponseHeader, setResponseHeader, headersSent, setContentType
    , setCookie, clearCookie
    , send, sendJson, sendJsonp
    , redirect, setLocation
    , sendFile, sendFileExt, download, downloadExt
    ) where


import Prelude
import Data.Maybe
import Data.Foreign.Class
import Data.Foreign.EasyFFI
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Unsafe
import Control.Monad
import Node.Express.Types
import Node.Express.Internal.Utils
import Node.Express.Internal.Response
import Node.Express.Internal.Request
import Node.Express.Internal.QueryString

--| Monad responsible for handling single request.
data HandlerM e a = HandlerM (Request -> Response -> ExpressM e Unit -> ExpressM e a)
type Handler e = HandlerM e Unit


instance functorHandlerM :: Functor (HandlerM e) where
    map f (HandlerM h) = HandlerM \req resp nxt ->
        (h req resp nxt >>= \r -> return $ f r)

instance applyHandlerM :: Apply (HandlerM e) where
    apply (HandlerM f) (HandlerM h) = HandlerM \req resp nxt -> do
        res   <- h req resp nxt
        trans <- f req resp nxt
        return $ trans res

instance applicativeHandlerM :: Applicative (HandlerM e) where
    pure x = HandlerM \_ _ _ -> return x

instance bindHandlerM :: Bind (HandlerM e) where
    bind (HandlerM h) f = HandlerM \req resp nxt -> do
        (HandlerM g) <- liftM1 f $ h req resp nxt
        g req resp nxt

instance monadHandlerM :: Monad (HandlerM e)

instance monadEffHandlerM :: MonadEff eff (HandlerM eff) where
    liftEff act = HandlerM \_ _ _ -> unsafeInterleaveEff act


withHandler :: forall e a. HandlerM e a -> Request -> Response -> ExpressM e Unit -> ExpressM e a
withHandler (HandlerM h) = h

--| Generate a closure from a function capturing current request and response.
--| It is intended to use with async functions like `fs.readFile`.
--| Example:
--|
--|     fileReadHandler :: Handler
--|     fileReadHandler = do
--|         callback <- capture $ \data ->
--|             send data
--|         fs.readFile("some_file.txt", callback)
--|
capture :: forall a b eff. (a -> HandlerM eff b) -> HandlerM eff (a -> Eff eff b)
capture fn = HandlerM \req resp nxt ->
    return $ \params -> unsafeInterleaveEff $ withHandler (fn params) req resp nxt

--| Call next handler/middleware in a chain.
next :: forall e. Handler e
next = HandlerM \_ _ nxt -> nxt

--| Call next handler/middleware and pass error to it.
nextThrow :: forall e a. Error -> HandlerM e a
nextThrow err = HandlerM \_ _ nxt ->
    intlNextWithError nxt err

-- Request --

--| Get route param value. If it is named route, e.g `/user/:id` then
--| `getRouteParam "id"` return matched part of route. If it is
--| regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
--| part that matched `(\d+)` and `getRouteParam 0` return whole
--| route.
getRouteParam :: forall e a. (RequestParam a) => a -> HandlerM e (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
    intlReqRouteParam req name

--| Get param from request's body.
--| NOTE: Not parsed by default, you must attach proper middleware
--|       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall e a. (IsForeign a) => String -> HandlerM e (Maybe a)
getBodyParam name = HandlerM \req _ _ ->
    intlReqBodyParam req name

--| Get param from query string (part of URL behind '?').
--| If there are multiple params having equal keys
--| return the first one.
getQueryParam :: forall e. String -> HandlerM e (Maybe String)
getQueryParam name = HandlerM \req _ _ -> do
    params <- intlReqQueryParams req
    return $ getOne params name

--| Get all params from query string having specified key.
getQueryParams :: forall e. String -> HandlerM e (Array String)
getQueryParams name = HandlerM \req _ _ -> do
    params <- intlReqQueryParams req
    return $ getAll params name

--| Return route that matched this request.
getRoute :: forall e. HandlerM e String
getRoute = HandlerM \req _ _ ->
    intlReqRoute req

--| Get cookie param by its key.
getCookie :: forall e. String -> HandlerM e (Maybe String)
getCookie name = HandlerM \req _ _ ->
    intlReqGetCookie req name

--| Get signed cookie param by its key.
getSignedCookie :: forall e. String -> HandlerM e (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    intlReqGetSignedCookie req name

--| Get request header param.
getRequestHeader :: forall e. String -> HandlerM e (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
    intlReqGetHeader req field

--| Check if specified response type will be accepted by a client.
accepts :: forall e. String -> HandlerM e (Maybe String)
accepts types = HandlerM \req _ _ ->
    intlReqAccepts req types

--| Execute specified handler if client accepts specified response type.
ifAccepts :: forall e. String -> Handler e -> Handler e
ifAccepts type_ act = do
    isAccepted <- (liftM1 (maybe false (const true)) $ accepts type_)
    when isAccepted act

--| Check if specified charset is accepted.
acceptsCharset :: forall e. String -> HandlerM e (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    intlReqAcceptsCharset req charset

--| Check if specified language is accepted.
acceptsLanguage :: forall e. String -> HandlerM e (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    intlReqAcceptsLanguage req language

--| Check if request's Content-Type field matches type.
--| See http://expressjs.com/4x/api.html#req.is
hasType :: forall e. String -> HandlerM e Boolean
hasType type_ = HandlerM \req _ _ ->
    intlReqHasType req type_

--| Return remote or upstream address.
getRemoteIp :: forall e. HandlerM e String
getRemoteIp = HandlerM \req _ _ ->
    intlReqGetRemoteIp req

--| Return list of X-Forwarded-For proxies if any.
getRemoteIps :: forall e. HandlerM e (Array String)
getRemoteIps = HandlerM \req _ _ ->
    intlReqGetRemoteIps req

--| Return request URL pathname.
getPath :: forall e. HandlerM e String
getPath = HandlerM \req _ _ ->
    intlReqGetPath req

--| Return Host header field.
getHostname :: forall e. HandlerM e String
getHostname = HandlerM \req _ _ ->
    intlReqGetHostname req

--| Return array of subdomains.
getSubdomains :: forall e. HandlerM e (Array String)
getSubdomains = HandlerM \req _ _ ->
    intlReqGetSubdomains req

--| Check that Last-Modified and/or ETag still matches.
isFresh :: forall e. HandlerM e Boolean
isFresh = HandlerM \req _ _ ->
    intlReqIsFresh req

--| Check that Last-Modified and/or ETag do not match.
isStale :: forall e. HandlerM e Boolean
isStale = HandlerM \req _ _ ->
    intlReqIsStale req

--| Check if request was issued by XMLHttpRequest.
isXhr :: forall e. HandlerM e Boolean
isXhr = HandlerM \req _ _ ->
    intlReqIsXhr req

--| Return request protocol.
getProtocol :: forall e. HandlerM e (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
    intlReqGetProtocol req

--| Return request HTTP method
getMethod :: forall e. HandlerM e (Maybe Method)
getMethod = HandlerM \req _ _ ->
    intlReqGetMethod req

--| Return request URL (may be modified by other handlers/middleware).
getUrl :: forall e. HandlerM e String
getUrl = HandlerM \req _ _ ->
    intlReqGetUrl req

--| Return request original URL.
getOriginalUrl :: forall e. HandlerM e String
getOriginalUrl = HandlerM \req _ _ ->
    intlReqGetOriginalUrl req

-- Response --

--| Set status code.
setStatus :: forall e. Int -> Handler e
setStatus val = HandlerM \_ resp _ ->
    intlRespSetStatus resp val

--| Return response header value.
getResponseHeader :: forall e a. (IsForeign a) => String -> HandlerM e (Maybe a)
getResponseHeader field = HandlerM \_ resp _ -> do
    intlRespGetHeader resp field

--| Set response header value.
setResponseHeader :: forall e a. String -> a -> Handler e
setResponseHeader field val = HandlerM \_ resp _ ->
    intlRespSetHeader resp field val

--| Check if headers have been sent already
headersSent :: forall e. HandlerM e Boolean
headersSent = HandlerM \_ resp _ ->
    intlRespHeadersSent resp

--| Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: forall e. String -> String -> CookieOptions -> Handler e
setCookie name val opts = HandlerM \_ resp _ ->
    intlRespSetCookie resp name val opts

--| Clear cookie.
clearCookie :: forall e. String -> String -> Handler e
clearCookie name path = HandlerM \_ resp _ ->
    intlRespClearCookie resp name path

--| Send a response. Could be object, string, buffer, etc.
send :: forall e a. a -> Handler e
send data_ = HandlerM \_ resp _ ->
    intlRespSend resp data_

--| Send a JSON response. Necessary headers are set automatically.
sendJson :: forall e a. a -> Handler e
sendJson data_ = HandlerM \_ resp _ ->
    intlRespSendJson resp data_

--| Send a JSON response with JSONP support.
sendJsonp :: forall e a. a -> Handler e
sendJsonp data_ = HandlerM \_ resp _ ->
    intlRespSendJsonp resp data_

--| Redirect to the given URL setting status to 302.
redirect :: forall e. String -> Handler e
redirect = redirectWithStatus 302

--| Redirect to the given URL using custom status.
redirectWithStatus :: forall e. Int -> String -> Handler e
redirectWithStatus status url = HandlerM \_ resp _ ->
    intlRespRedirect resp status url

--| Set Location header.
setLocation :: forall e. String -> Handler e
setLocation url = HandlerM \_ resp _ ->
    intlRespSetLocation resp url

--| Set Content-Type header.
setContentType :: forall e. String -> Handler e
setContentType t = HandlerM \_ resp _ ->
    intlRespType resp t

--| Send file by its path.
sendFile :: forall e. String -> Handler e
sendFile path = sendFileExt path {root: pwd} (\_ -> return unit)
  where
    pwd = unsafeForeignFunction [] "process.cwd()"

--| Send file by its path using specified options and error handler.
--| See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall e o. String -> { | o } -> (Error -> ExpressM e Unit) -> Handler e
sendFileExt path opts callback = HandlerM \_ resp _ ->
    intlRespSendFile resp path opts callback

--| Transfer file as an attachment (will prompt user to download).
download :: forall e. String -> Handler e
download path = downloadExt path "" (\_ -> return unit)

--| Transfer file as an attachment using specified filename and error handler.
downloadExt :: forall e. String -> String -> (Error -> ExpressM e Unit) -> Handler e
downloadExt path filename callback = HandlerM \_ resp _ ->
    intlRespDownload resp path filename callback
