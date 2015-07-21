module Node.Express.Handler
    ( HandlerM()
    , Handler()
    , ExpressHandlerM()
    , runHandlerM, next, nextThrow
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
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.EasyFFI
import Control.Monad.Aff
import Control.Monad.Aff.Class
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

-- | Monad responsible for handling single request.
data HandlerM e a = HandlerM (Request -> Response -> Eff e Unit -> Aff e a)

type ExpressHandlerM e = HandlerM (express :: EXPRESS | e)
type Handler e = ExpressHandlerM e Unit

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
    liftEff act = HandlerM \_ _ _ -> liftEff act

instance monadAffHandlerM :: MonadAff eff (HandlerM eff) where
    liftAff act = HandlerM \_ _ _ -> act

runHandlerM :: forall e a. HandlerM e a -> Request -> Response -> Eff e Unit -> Eff e Unit
runHandlerM (HandlerM h) req res next = launchAff (h req res next)

-- | Call next handler/middleware in a chain.
next :: forall e. Handler e
next = HandlerM \_ _ nxt -> liftEff nxt

-- | Call next handler/middleware and pass error to it.
nextThrow :: forall e a. Error -> ExpressHandlerM e a
nextThrow err = HandlerM \_ _ nxt ->
    liftEff $ intlNextWithError nxt err

-- Request --

-- | Get route param value. If it is named route, e.g `/user/:id` then
-- | `getRouteParam "id"` return matched part of route. If it is
-- | regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
-- | part that matched `(\d+)` and `getRouteParam 0` return whole
-- | route.
getRouteParam :: forall e a. (RequestParam a) => a -> ExpressHandlerM e (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
    liftEff $ intlReqRouteParam req name

-- | Get param from request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall e a. (IsForeign a) => String -> ExpressHandlerM e (Maybe a)
getBodyParam name = HandlerM \req _ _ ->
    liftEff $ intlReqBodyParam req name

-- | Get param from query string (part of URL behind '?').
-- | If there are multiple params having equal keys
-- | return the first one.
getQueryParam :: forall e. String -> ExpressHandlerM e (Maybe String)
getQueryParam name = HandlerM \req _ _ -> do
    params <- liftEff $ intlReqQueryParams req
    return $ getOne params name

-- | Get all params from query string having specified key.
getQueryParams :: forall e. String -> ExpressHandlerM e (Array String)
getQueryParams name = HandlerM \req _ _ -> do
    params <- liftEff $ intlReqQueryParams req
    return $ getAll params name

-- | Return route that matched this request.
getRoute :: forall e. ExpressHandlerM e String
getRoute = HandlerM \req _ _ ->
    liftEff $ reqRoute req

foreign import reqRoute :: forall e. Request -> ExpressM e String

-- | Get cookie param by its key.
getCookie :: forall e. String -> ExpressHandlerM e (Maybe String)
getCookie name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_getCookie req name)

foreign import _getCookie :: forall e. Request -> String -> ExpressM e Foreign

-- | Get signed cookie param by its key.
getSignedCookie :: forall e. String -> ExpressHandlerM e (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_getSignedCookie req name)

foreign import _getSignedCookie :: forall e. Request -> String -> ExpressM e Foreign
                                   
-- | Get request header param.
getRequestHeader :: forall e. String -> ExpressHandlerM e (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_getHeader req field)

foreign import _getHeader :: forall e. Request -> String -> ExpressM e Foreign

-- | Check if specified response type will be accepted by a client.
accepts :: forall e. String -> ExpressHandlerM e (Maybe String)
accepts types = HandlerM \req _ _ ->
    liftEff $ intlReqAccepts req types

-- | Execute specified handler if client accepts specified response type.
ifAccepts :: forall e. String -> Handler e -> Handler e
ifAccepts type_ act = do
    isAccepted <- (liftM1 (maybe false (const true)) $ accepts type_)
    when isAccepted act

-- | Check if specified charset is accepted.
acceptsCharset :: forall e. String -> ExpressHandlerM e (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    liftEff $ intlReqAcceptsCharset req charset

-- | Check if specified language is accepted.
acceptsLanguage :: forall e. String -> ExpressHandlerM e (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    liftEff $ intlReqAcceptsLanguage req language

-- | Check if request's Content-Type field matches type.
-- | See http://expressjs.com/4x/api.html#req.is
hasType :: forall e. String -> ExpressHandlerM e Boolean
hasType type_ = HandlerM \req _ _ ->
    liftEff $ intlReqHasType req type_

-- | Return remote or upstream address.
getRemoteIp :: forall e. ExpressHandlerM e String
getRemoteIp = HandlerM \req _ _ ->
    liftEff $ intlReqGetRemoteIp req

-- | Return list of X-Forwarded-For proxies if any.
getRemoteIps :: forall e. ExpressHandlerM e (Array String)
getRemoteIps = HandlerM \req _ _ ->
    liftEff $ intlReqGetRemoteIps req

-- | Return request URL pathname.
getPath :: forall e. ExpressHandlerM e String
getPath = HandlerM \req _ _ ->
    liftEff $ intlReqGetPath req

-- | Return Host header field.
getHostname :: forall e. ExpressHandlerM e String
getHostname = HandlerM \req _ _ ->
    liftEff $ intlReqGetHostname req

-- | Return array of subdomains.
getSubdomains :: forall e. ExpressHandlerM e (Array String)
getSubdomains = HandlerM \req _ _ ->
    liftEff $ intlReqGetSubdomains req

-- | Check that Last-Modified and/or ETag still matches.
isFresh :: forall e. ExpressHandlerM e Boolean
isFresh = HandlerM \req _ _ ->
    liftEff $ intlReqIsFresh req

-- | Check that Last-Modified and/or ETag do not match.
isStale :: forall e. ExpressHandlerM e Boolean
isStale = HandlerM \req _ _ ->
    liftEff $ intlReqIsStale req

-- | Check if request was issued by XMLHttpRequest.
isXhr :: forall e. ExpressHandlerM e Boolean
isXhr = HandlerM \req _ _ ->
    liftEff $ intlReqIsXhr req

-- | Return request protocol.
getProtocol :: forall e. ExpressHandlerM e (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
    liftEff $ intlReqGetProtocol req

-- | Return request HTTP method
getMethod :: forall e. ExpressHandlerM e (Maybe Method)
getMethod = HandlerM \req _ _ ->
    liftEff $ intlReqGetMethod req

-- | Return request URL (may be modified by other handlers/middleware).
getUrl :: forall e. ExpressHandlerM e String
getUrl = HandlerM \req _ _ ->
    liftEff $ intlReqGetUrl req

-- | Return request original URL.
getOriginalUrl :: forall e. ExpressHandlerM e String
getOriginalUrl = HandlerM \req _ _ ->
    liftEff $ intlReqGetOriginalUrl req

-- Response --

-- | Set status code.
setStatus :: forall e. Int -> Handler e
setStatus val = HandlerM \_ resp _ ->
    liftEff $ intlRespSetStatus resp val

-- | Return response header value.
getResponseHeader :: forall e a. (IsForeign a) => String -> ExpressHandlerM e (Maybe a)
getResponseHeader field = HandlerM \_ resp _ -> do
    liftEff $ intlRespGetHeader resp field

-- | Set response header value.
setResponseHeader :: forall e a. String -> a -> Handler e
setResponseHeader field val = HandlerM \_ resp _ ->
    liftEff $ intlRespSetHeader resp field val

-- | Check if headers have been sent already
headersSent :: forall e. ExpressHandlerM e Boolean
headersSent = HandlerM \_ resp _ ->
    liftEff $ intlRespHeadersSent resp

-- | Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: forall e. String -> String -> CookieOptions -> Handler e
setCookie name val opts = HandlerM \_ resp _ ->
    liftEff $ intlRespSetCookie resp name val opts

-- | Clear cookie.
clearCookie :: forall e. String -> String -> Handler e
clearCookie name path = HandlerM \_ resp _ ->
    liftEff $ intlRespClearCookie resp name path

-- | Send a response. Could be object, string, buffer, etc.
send :: forall e a. a -> Handler e
send data_ = HandlerM \_ resp _ ->
    liftEff $ intlRespSend resp data_

-- | Send a JSON response. Necessary headers are set automatically.
sendJson :: forall e a. a -> Handler e
sendJson data_ = HandlerM \_ resp _ ->
    liftEff $ intlRespSendJson resp data_

-- | Send a JSON response with JSONP support.
sendJsonp :: forall e a. a -> Handler e
sendJsonp data_ = HandlerM \_ resp _ ->
    liftEff $ intlRespSendJsonp resp data_

-- | Redirect to the given URL setting status to 302.
redirect :: forall e. String -> Handler e
redirect = redirectWithStatus 302

-- | Redirect to the given URL using custom status.
redirectWithStatus :: forall e. Int -> String -> Handler e
redirectWithStatus status url = HandlerM \_ resp _ ->
    liftEff $ intlRespRedirect resp status url

-- | Set Location header.
setLocation :: forall e. String -> Handler e
setLocation url = HandlerM \_ resp _ ->
    liftEff $ intlRespSetLocation resp url

-- | Set Content-Type header.
setContentType :: forall e. String -> Handler e
setContentType t = HandlerM \_ resp _ ->
    liftEff $ intlRespType resp t

-- | Send file by its path.
sendFile :: forall e. String -> Handler e
sendFile path = sendFileExt path {root: pwd} (\_ -> return unit)
  where
    pwd = unsafeForeignFunction [] "process.cwd()"

-- | Send file by its path using specified options and error handler.
-- | See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall e o. String -> { | o } -> (Error -> ExpressM e Unit) -> Handler e
sendFileExt path opts callback = HandlerM \_ resp _ ->
    liftEff $ intlRespSendFile resp path opts callback

-- | Transfer file as an attachment (will prompt user to download).
download :: forall e. String -> Handler e
download path = downloadExt path "" (\_ -> return unit)

-- | Transfer file as an attachment using specified filename and error handler.
downloadExt :: forall e. String -> String -> (Error -> ExpressM e Unit) -> Handler e
downloadExt path filename callback = HandlerM \_ resp _ ->
    liftEff $ intlRespDownload resp path filename callback
