module Node.Express.Handler
    ( HandlerM()
    , Handler()
    , HandlerAff()
    , runHandlerAff, next, nextThrow
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
import Control.Monad.Aff
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

data HandlerAff e a = HandlerAff (Request -> Response -> Eff e Unit -> Aff e a)
type Handler e = HandlerAff e Unit

instance functorHandlerM :: Functor (HandlerAff e) where
    map f (HandlerAff h) = HandlerAff \req resp nxt ->
        (h req resp nxt >>= \r -> return $ f r)

instance applyHandlerM :: Apply (HandlerAff e) where
    apply (HandlerAff f) (HandlerAff h) = HandlerAff \req resp nxt -> do
        res   <- h req resp nxt
        trans <- f req resp nxt
        return $ trans res

instance applicativeHandlerM :: Applicative (HandlerAff e) where
    pure x = HandlerAff \_ _ _ -> return x

instance bindHandlerM :: Bind (HandlerAff e) where
    bind (HandlerAff h) f = HandlerAff \req resp nxt -> do
        (HandlerAff g) <- liftM1 f $ h req resp nxt
        g req resp nxt

instance monadHandlerM :: Monad (HandlerAff e)

instance monadEffHandlerM :: MonadEff eff (HandlerAff eff) where
    liftEff act = HandlerAff \_ _ _ -> liftEff act

runHandlerAff :: forall e a. HandlerAff e a -> Request -> Response -> Eff e Unit -> Eff e Unit
runHandlerAff (HandlerAff h) req res next = launchAff (h req res next)

--| Call next handler/middleware in a chain.
next :: forall e. HandlerAff (express :: EXPRESS | e) Unit
next = HandlerAff \_ _ nxt -> liftEff nxt

--| Call next handler/middleware and pass error to it.
nextThrow :: forall e a. Error -> HandlerAff (express :: EXPRESS | e) a
nextThrow err = HandlerAff \_ _ nxt ->
    liftEff $ intlNextWithError nxt err

-- Request --

--| Get route param value. If it is named route, e.g `/user/:id` then
--| `getRouteParam "id"` return matched part of route. If it is
--| regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
--| part that matched `(\d+)` and `getRouteParam 0` return whole
--| route.
getRouteParam :: forall e a. (RequestParam a) => a -> HandlerAff (express :: EXPRESS | e) (Maybe String)
getRouteParam name = HandlerAff \req _ _ ->
    liftEff $ intlReqRouteParam req name

--| Get param from request's body.
--| NOTE: Not parsed by default, you must attach proper middleware
--|       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall e a. (IsForeign a) => String -> HandlerAff (express :: EXPRESS | e) (Maybe a)
getBodyParam name = HandlerAff \req _ _ ->
    liftEff $ intlReqBodyParam req name

--| Get param from query string (part of URL behind '?').
--| If there are multiple params having equal keys
--| return the first one.
getQueryParam :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
getQueryParam name = HandlerAff \req _ _ -> do
    params <- liftEff $ intlReqQueryParams req
    return $ getOne params name

--| Get all params from query string having specified key.
getQueryParams :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Array String)
getQueryParams name = HandlerAff \req _ _ -> do
    params <- liftEff $ intlReqQueryParams req
    return $ getAll params name

--| Return route that matched this request.
getRoute :: forall e. HandlerAff (express :: EXPRESS | e) String
getRoute = HandlerAff \req _ _ ->
    liftEff $ intlReqRoute req

--| Get cookie param by its key.
getCookie :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
getCookie name = HandlerAff \req _ _ ->
    liftEff $ intlReqGetCookie req name

--| Get signed cookie param by its key.
getSignedCookie :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
getSignedCookie name = HandlerAff \req _ _ ->
    liftEff $ intlReqGetSignedCookie req name

--| Get request header param.
getRequestHeader :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
getRequestHeader field = HandlerAff \req _ _ ->
    liftEff $ intlReqGetHeader req field

--| Check if specified response type will be accepted by a client.
accepts :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
accepts types = HandlerAff \req _ _ ->
    liftEff $ intlReqAccepts req types

--| Execute specified handler if client accepts specified response type.
ifAccepts :: forall e. String -> Handler (express :: EXPRESS | e) -> HandlerAff (express :: EXPRESS | e) Unit
ifAccepts type_ act = do
    isAccepted <- (liftM1 (maybe false (const true)) $ accepts type_)
    when isAccepted act

--| Check if specified charset is accepted.
acceptsCharset :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
acceptsCharset charset = HandlerAff \req _ _ ->
    liftEff $ intlReqAcceptsCharset req charset

--| Check if specified language is accepted.
acceptsLanguage :: forall e. String -> HandlerAff (express :: EXPRESS | e) (Maybe String)
acceptsLanguage language = HandlerAff \req _ _ ->
    liftEff $ intlReqAcceptsLanguage req language

--| Check if request's Content-Type field matches type.
--| See http://expressjs.com/4x/api.html#req.is
hasType :: forall e. String -> HandlerAff (express :: EXPRESS | e) Boolean
hasType type_ = HandlerAff \req _ _ ->
    liftEff $ intlReqHasType req type_

--| Return remote or upstream address.
getRemoteIp :: forall e. HandlerAff (express :: EXPRESS | e) String
getRemoteIp = HandlerAff \req _ _ ->
    liftEff $ intlReqGetRemoteIp req

--| Return list of X-Forwarded-For proxies if any.
getRemoteIps :: forall e. HandlerAff (express :: EXPRESS | e) (Array String)
getRemoteIps = HandlerAff \req _ _ ->
    liftEff $ intlReqGetRemoteIps req

--| Return request URL pathname.
getPath :: forall e. HandlerAff (express :: EXPRESS | e) String
getPath = HandlerAff \req _ _ ->
    liftEff $ intlReqGetPath req

--| Return Host header field.
getHostname :: forall e. HandlerAff (express :: EXPRESS | e) String
getHostname = HandlerAff \req _ _ ->
    liftEff $ intlReqGetHostname req

--| Return array of subdomains.
getSubdomains :: forall e. HandlerAff (express :: EXPRESS | e) (Array String)
getSubdomains = HandlerAff \req _ _ ->
    liftEff $ intlReqGetSubdomains req

--| Check that Last-Modified and/or ETag still matches.
isFresh :: forall e. HandlerAff (express :: EXPRESS | e) Boolean
isFresh = HandlerAff \req _ _ ->
    liftEff $ intlReqIsFresh req

--| Check that Last-Modified and/or ETag do not match.
isStale :: forall e. HandlerAff (express :: EXPRESS | e) Boolean
isStale = HandlerAff \req _ _ ->
    liftEff $ intlReqIsStale req

--| Check if request was issued by XMLHttpRequest.
isXhr :: forall e. HandlerAff (express :: EXPRESS | e) Boolean
isXhr = HandlerAff \req _ _ ->
    liftEff $ intlReqIsXhr req

--| Return request protocol.
getProtocol :: forall e. HandlerAff (express :: EXPRESS | e) (Maybe Protocol)
getProtocol = HandlerAff \req _ _ ->
    liftEff $ intlReqGetProtocol req

--| Return request HTTP method
getMethod :: forall e. HandlerAff (express :: EXPRESS | e) (Maybe Method)
getMethod = HandlerAff \req _ _ ->
    liftEff $ intlReqGetMethod req

--| Return request URL (may be modified by other handlers/middleware).
getUrl :: forall e. HandlerAff (express :: EXPRESS | e) String
getUrl = HandlerAff \req _ _ ->
    liftEff $ intlReqGetUrl req

--| Return request original URL.
getOriginalUrl :: forall e. HandlerAff (express :: EXPRESS | e) String
getOriginalUrl = HandlerAff \req _ _ ->
    liftEff $ intlReqGetOriginalUrl req

-- Response --

--| Set status code.
setStatus :: forall e. Int -> HandlerAff (express :: EXPRESS | e) Unit
setStatus val = HandlerAff \_ resp _ ->
    liftEff $ intlRespSetStatus resp val

--| Return response header value.
getResponseHeader :: forall e a. (IsForeign a) => String -> HandlerAff (express :: EXPRESS | e) (Maybe a)
getResponseHeader field = HandlerAff \_ resp _ -> do
    liftEff $ intlRespGetHeader resp field

--| Set response header value.
setResponseHeader :: forall e a. String -> a -> HandlerAff (express :: EXPRESS | e) Unit
setResponseHeader field val = HandlerAff \_ resp _ ->
    liftEff $ intlRespSetHeader resp field val

--| Check if headers have been sent already
headersSent :: forall e. HandlerAff (express :: EXPRESS | e) Boolean
headersSent = HandlerAff \_ resp _ ->
    liftEff $ intlRespHeadersSent resp

--| Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: forall e. String -> String -> CookieOptions -> HandlerAff (express :: EXPRESS | e) Unit
setCookie name val opts = HandlerAff \_ resp _ ->
    liftEff $ intlRespSetCookie resp name val opts

--| Clear cookie.
clearCookie :: forall e. String -> String -> HandlerAff (express :: EXPRESS | e) Unit
clearCookie name path = HandlerAff \_ resp _ ->
    liftEff $ intlRespClearCookie resp name path

--| Send a response. Could be object, string, buffer, etc.
send :: forall e a. a -> HandlerAff (express :: EXPRESS | e) Unit
send data_ = HandlerAff \_ resp _ ->
    liftEff $ intlRespSend resp data_

--| Send a JSON response. Necessary headers are set automatically.
sendJson :: forall e a. a -> HandlerAff (express :: EXPRESS | e) Unit
sendJson data_ = HandlerAff \_ resp _ ->
    liftEff $ intlRespSendJson resp data_

--| Send a JSON response with JSONP support.
sendJsonp :: forall e a. a -> HandlerAff (express :: EXPRESS | e) Unit
sendJsonp data_ = HandlerAff \_ resp _ ->
    liftEff $ intlRespSendJsonp resp data_

--| Redirect to the given URL setting status to 302.
redirect :: forall e. String -> HandlerAff (express :: EXPRESS | e) Unit
redirect = redirectWithStatus 302

--| Redirect to the given URL using custom status.
redirectWithStatus :: forall e. Int -> String -> HandlerAff (express :: EXPRESS | e) Unit
redirectWithStatus status url = HandlerAff \_ resp _ ->
    liftEff $ intlRespRedirect resp status url

--| Set Location header.
setLocation :: forall e. String -> HandlerAff (express :: EXPRESS | e) Unit
setLocation url = HandlerAff \_ resp _ ->
    liftEff $ intlRespSetLocation resp url

--| Set Content-Type header.
setContentType :: forall e. String -> HandlerAff (express :: EXPRESS | e) Unit
setContentType t = HandlerAff \_ resp _ ->
    liftEff $ intlRespType resp t

--| Send file by its path.
sendFile :: forall e. String -> HandlerAff (express :: EXPRESS | e) Unit
sendFile path = sendFileExt path {root: pwd} (\_ -> return unit)
  where
    pwd = unsafeForeignFunction [] "process.cwd()"

--| Send file by its path using specified options and error handler.
--| See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall e o. String -> { | o } -> (Error -> ExpressM e Unit) -> HandlerAff (express :: EXPRESS | e) Unit
sendFileExt path opts callback = HandlerAff \_ resp _ ->
    liftEff $ intlRespSendFile resp path opts callback

--| Transfer file as an attachment (will prompt user to download).
download :: forall e. String -> HandlerAff (express :: EXPRESS | e) Unit
download path = downloadExt path "" (\_ -> return unit)

--| Transfer file as an attachment using specified filename and error handler.
downloadExt :: forall e. String -> String -> (Error -> ExpressM e Unit) -> HandlerAff (express :: EXPRESS | e) Unit
downloadExt path filename callback = HandlerAff \_ resp _ ->
    liftEff $ intlRespDownload resp path filename callback
