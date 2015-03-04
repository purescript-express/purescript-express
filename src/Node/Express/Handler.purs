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
data HandlerM a = HandlerM (Request -> Response -> ExpressM Unit -> ExpressM a)
type Handler = HandlerM Unit


instance functorHandlerM :: Functor HandlerM where
    (<$>) f (HandlerM h) = HandlerM \req resp nxt ->
        (h req resp nxt >>= \r -> return $ f r)

instance applyHandlerM :: Apply HandlerM where
    (<*>) (HandlerM f) (HandlerM h) = HandlerM \req resp nxt -> do
        res   <- h req resp nxt
        trans <- f req resp nxt
        return $ trans res

instance applicativeHandlerM :: Applicative HandlerM where
    pure x = HandlerM \_ _ _ -> return x

instance bindHandlerM :: Bind HandlerM where
    (>>=) (HandlerM h) f = HandlerM \req resp nxt -> do
        (HandlerM g) <- liftM1 f $ h req resp nxt
        g req resp nxt

instance monadHandlerM :: Monad HandlerM

instance monadEffHandlerM :: MonadEff eff HandlerM where
    liftEff act = HandlerM \_ _ _ -> unsafeInterleaveEff act


withHandler :: forall a. HandlerM a -> Request -> Response -> ExpressM Unit -> ExpressM a
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
capture :: forall a b eff. (a -> HandlerM b) -> HandlerM (a -> Eff eff b)
capture fn = HandlerM \req resp nxt ->
    return $ \params -> unsafeInterleaveEff $ withHandler (fn params) req resp nxt

--| Call next handler/middleware in a chain.
next :: Handler
next = HandlerM \_ _ nxt -> nxt

--| Call next handler/middleware and pass error to it.
nextThrow :: Error -> Handler
nextThrow err = HandlerM \_ _ nxt ->
    intlNextWithError nxt err

-- Request --

--| Get route param value. If it is named route, e.g `/user/:id` then
--| `getRouteParam "id"` return matched part of route. If it is
--| regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
--| part that matched `(\d+)` and `getRouteParam 0` return whole
--| route.
getRouteParam :: forall a. (RequestParam a) => a -> HandlerM (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
    intlReqRouteParam req name

--| Get param from request's body.
--| NOTE: Not parsed by default, you must attach proper middleware
--|       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall a. (IsForeign a) => String -> HandlerM (Maybe a)
getBodyParam name = HandlerM \req _ _ ->
    intlReqBodyParam req name

--| Get param from query string (part of URL behind '?').
--| If there are multiple params having equal keys
--| return the first one.
getQueryParam :: String -> HandlerM (Maybe String)
getQueryParam name = HandlerM \req _ _ -> do
    params <- intlReqQueryParams req
    return $ getOne params name

--| Get all params from query string having specified key.
getQueryParams :: String -> HandlerM [String]
getQueryParams name = HandlerM \req _ _ -> do
    params <- intlReqQueryParams req
    return $ getAll params name

--| Return route that matched this request.
getRoute :: HandlerM String
getRoute = HandlerM \req _ _ ->
    intlReqRoute req

--| Get cookie param by its key.
getCookie :: String -> HandlerM (Maybe String)
getCookie name = HandlerM \req _ _ ->
    intlReqGetCookie req name

--| Get signed cookie param by its key.
getSignedCookie :: String -> HandlerM (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    intlReqGetSignedCookie req name

--| Get request header param.
getRequestHeader :: String -> HandlerM (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
    intlReqGetHeader req field

--| Check if specified response type will be accepted by a client.
accepts :: String -> HandlerM (Maybe String)
accepts types = HandlerM \req _ _ ->
    intlReqAccepts req types

--| Execute specified handler if client accepts specified response type.
ifAccepts :: String -> Handler -> Handler
ifAccepts type_ act = do
    isAccepted <- (liftM1 (maybe false (const true)) $ accepts type_)
    when isAccepted act

--| Check if specified charset is accepted.
acceptsCharset :: String -> HandlerM (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    intlReqAcceptsCharset req charset

--| Check if specified language is accepted.
acceptsLanguage :: String -> HandlerM (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    intlReqAcceptsLanguage req language

--| Check if request's Content-Type field matches type.
--| See http://expressjs.com/4x/api.html#req.is
hasType :: String -> HandlerM Boolean
hasType type_ = HandlerM \req _ _ ->
    intlReqHasType req type_

--| Return remote or upstream address.
getRemoteIp :: HandlerM String
getRemoteIp = HandlerM \req _ _ ->
    intlReqGetRemoteIp req

--| Return list of X-Forwarded-For proxies if any.
getRemoteIps :: HandlerM [String]
getRemoteIps = HandlerM \req _ _ ->
    intlReqGetRemoteIps req

--| Return request URL pathname.
getPath :: HandlerM String
getPath = HandlerM \req _ _ ->
    intlReqGetPath req

--| Return Host header field.
getHostname :: HandlerM String
getHostname = HandlerM \req _ _ ->
    intlReqGetHostname req

--| Return array of subdomains.
getSubdomains :: HandlerM [String]
getSubdomains = HandlerM \req _ _ ->
    intlReqGetSubdomains req

--| Check that Last-Modified and/or ETag still matches.
isFresh :: HandlerM Boolean
isFresh = HandlerM \req _ _ ->
    intlReqIsFresh req

--| Check that Last-Modified and/or ETag do not match.
isStale :: HandlerM Boolean
isStale = HandlerM \req _ _ ->
    intlReqIsStale req

--| Check if request was issued by XMLHttpRequest.
isXhr :: HandlerM Boolean
isXhr = HandlerM \req _ _ ->
    intlReqIsXhr req

--| Return request protocol.
getProtocol :: HandlerM (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
    intlReqGetProtocol req

--| Return request HTTP method
getMethod :: HandlerM (Maybe Method)
getMethod = HandlerM \req _ _ ->
    intlReqGetMethod req

--| Return request URL (may be modified by other handlers/middleware).
getUrl :: HandlerM String
getUrl = HandlerM \req _ _ ->
    intlReqGetUrl req

--| Return request original URL.
getOriginalUrl :: HandlerM String
getOriginalUrl = HandlerM \req _ _ ->
    intlReqGetOriginalUrl req

-- Response --

--| Set status code.
setStatus :: Number -> Handler
setStatus val = HandlerM \_ resp _ ->
    intlRespSetStatus resp val

--| Return response header value.
getResponseHeader :: forall a. (IsForeign a) => String -> HandlerM (Maybe a)
getResponseHeader field = HandlerM \_ resp _ -> do
    intlRespGetHeader resp field

--| Set response header value.
setResponseHeader :: forall a. String -> a -> Handler
setResponseHeader field val = HandlerM \_ resp _ ->
    intlRespSetHeader resp field val

--| Check if headers have been sent already
headersSent :: HandlerM Boolean
headersSent = HandlerM \_ resp _ ->
    intlRespHeadersSent resp

--| Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: String -> String -> CookieOptions -> Handler
setCookie name val opts = HandlerM \_ resp _ ->
    intlRespSetCookie resp name val opts

--| Clear cookie.
clearCookie :: String -> String -> Handler
clearCookie name path = HandlerM \_ resp _ ->
    intlRespClearCookie resp name path

--| Send a response. Could be object, string, buffer, etc.
send :: forall a. a -> Handler
send data_ = HandlerM \_ resp _ ->
    intlRespSend resp data_

--| Send a JSON response. Necessary headers are set automatically.
sendJson :: forall a. a -> Handler
sendJson data_ = HandlerM \_ resp _ ->
    intlRespSendJson resp data_

--| Send a JSON response with JSONP support.
sendJsonp :: forall a. a -> Handler
sendJsonp data_ = HandlerM \_ resp _ ->
    intlRespSendJsonp resp data_

--| Redirect to the given URL setting status to 302.
redirect :: String -> Handler
redirect = redirectWithStatus 302

--| Redirect to the given URL using custom status.
redirectWithStatus :: Number -> String -> Handler
redirectWithStatus status url = HandlerM \_ resp _ ->
    intlRespRedirect resp status url

--| Set Location header.
setLocation :: String -> Handler
setLocation url = HandlerM \_ resp _ ->
    intlRespSetLocation resp url

--| Set Content-Type header.
setContentType :: String -> Handler
setContentType t = HandlerM \_ resp _ ->
    intlRespType resp t

--| Send file by its path.
sendFile :: String -> Handler
sendFile path = sendFileExt path {root: pwd} (\_ -> return unit)
  where
    pwd = unsafeForeignFunction [] "process.cwd()"

--| Send file by its path using specified options and error handler.
--| See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall o. String -> { | o } -> (Error -> ExpressM Unit) -> Handler
sendFileExt path opts callback = HandlerM \_ resp _ ->
    intlRespSendFile resp path opts callback

--| Transfer file as an attachment (will prompt user to download).
download :: String -> Handler
download path = downloadExt path "" (\_ -> return unit)

--| Transfer file as an attachment using specified filename and error handler.
downloadExt :: String -> String -> (Error -> ExpressM Unit) -> Handler
downloadExt path filename callback = HandlerM \_ resp _ ->
    intlRespDownload resp path filename callback
