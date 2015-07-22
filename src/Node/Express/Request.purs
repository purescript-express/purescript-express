module Node.Express.Request
    ( getRouteParam, getQueryParam, getQueryParams, getBodyParam
    , getRoute
    , getCookie, getSignedCookie
    , getRequestHeader
    , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
    , getRemoteIp, getRemoteIps, getPath, getHostname, getSubdomains
    , isFresh, isStale
    , isXhr, getProtocol, getMethod
    , getUrl, getOriginalUrl
    ) where

import Prelude
import Data.Foreign
import Data.Foreign.EasyFFI
import Data.Foreign.Class
import Data.Either
import Data.Maybe
import Control.Monad (when)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Node.Express.Handler
import Node.Express.Types
import Node.Express.Internal.Utils
import Node.Express.Internal.QueryString

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
    liftEff $ _getRoute req

-- | Get cookie param by its key.
getCookie :: forall e. String -> ExpressHandlerM e (Maybe String)
getCookie name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_getCookie req name)

-- | Get signed cookie param by its key.
getSignedCookie :: forall e. String -> ExpressHandlerM e (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_getSignedCookie req name)
                                   
-- | Get request header param.
getRequestHeader :: forall e. String -> ExpressHandlerM e (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_getHeader req field)

-- | Check if specified response type will be accepted by a client.
accepts :: forall e. String -> ExpressHandlerM e (Maybe String)
accepts types = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_accepts req types)

-- | Execute specified handler if client accepts specified response type.
ifAccepts :: forall e. String -> Handler e -> Handler e
ifAccepts type_ act = do
    isAccepted <- (liftM1 (maybe false (const true)) $ accepts type_)
    when isAccepted act

-- | Check if specified charset is accepted.
acceptsCharset :: forall e. String -> ExpressHandlerM e (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_acceptsCharset req charset)

-- | Check if specified language is accepted.
acceptsLanguage :: forall e. String -> ExpressHandlerM e (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< read) (_acceptsLanguage req language)

-- | Check if request's Content-Type field matches type.
-- | See http://expressjs.com/4x/api.html#req.is
hasType :: forall e. String -> ExpressHandlerM e Boolean
hasType type_ = HandlerM \req _ _ -> do
    val <- liftEff $ liftM1 (eitherToMaybe <<< read) (_hasType req type_)
    return $ fromMaybe false val

foreign import _getRoute :: forall e. Request -> ExpressM e String
foreign import _getCookie :: forall e. Request -> String -> ExpressM e Foreign
foreign import _getSignedCookie :: forall e. Request -> String -> ExpressM e Foreign
foreign import _getHeader :: forall e. Request -> String -> ExpressM e Foreign
foreign import _accepts :: forall e. Request -> String -> ExpressM e Foreign
foreign import _acceptsCharset :: forall e. Request -> String -> ExpressM e Foreign
foreign import _acceptsLanguage :: forall e. Request -> String -> ExpressM e Foreign
foreign import _hasType :: forall e. Request -> String -> ExpressM e Foreign

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


intlReqRouteParam ::
    forall e a. (RequestParam a) =>
    Request -> a -> ExpressM e (Maybe String)
intlReqRouteParam req name = do
    let getter :: forall e a. Request -> a -> ExpressM e Foreign
        getter = unsafeForeignFunction ["req", "name", ""] "req.params[name]"
    liftM1 (eitherToMaybe <<< read) (getter req name)

intlReqBodyParam ::
    forall e a. (IsForeign a) =>
    Request -> String -> ExpressM e (Maybe a)
intlReqBodyParam req name = do
    let getter = unsafeForeignFunction ["req", "name", ""]
                    "req.body == null ? void 0 : req.body[name]"
    liftM1 (eitherToMaybe <<< read) (getter req name)

intlReqQueryParams :: forall e. Request -> ExpressM e (Array Param)
intlReqQueryParams req = do
    let getter = unsafeForeignFunction ["req", ""] "req.url.split('?')[1] || ''"
    query <- getter req
    case parse query of
        Left _ -> return []
        Right params -> return params





intlReqGetRemoteIp :: forall e. Request -> ExpressM e String
intlReqGetRemoteIp = unsafeForeignFunction ["req", ""] "req.ip"

intlReqGetRemoteIps :: forall e. Request -> ExpressM e (Array String)
intlReqGetRemoteIps = unsafeForeignFunction ["req", ""] "req.ips"

intlReqGetPath :: forall e. Request -> ExpressM e String
intlReqGetPath = unsafeForeignFunction ["req", ""] "req.path"

intlReqGetHostname :: forall e. Request -> ExpressM e String
intlReqGetHostname = unsafeForeignFunction ["req", ""] "req.hostname"

intlReqGetSubdomains :: forall e. Request -> ExpressM e (Array String)
intlReqGetSubdomains = unsafeForeignFunction ["req", ""] "req.subdomains"


intlReqIsFresh :: forall e. Request -> ExpressM e Boolean
intlReqIsFresh = unsafeForeignFunction ["req", ""] "req.fresh"

intlReqIsStale :: forall e. Request -> ExpressM e Boolean
intlReqIsStale = unsafeForeignFunction ["req", ""] "req.stale"


intlReqIsXhr :: forall e. Request -> ExpressM e Boolean
intlReqIsXhr = unsafeForeignFunction ["req", ""] "req.xhr"

intlReqGetProtocol :: forall e. Request -> ExpressM e (Maybe Protocol)
intlReqGetProtocol req = do
    let getter = unsafeForeignFunction ["req", ""] "req.protocol"
    liftM1 (eitherToMaybe <<< read) (getter req)

intlReqGetMethod :: forall e. Request -> ExpressM e (Maybe Method)
intlReqGetMethod req = do
    let getter = unsafeForeignFunction ["req", ""] "req.method"
    liftM1 (eitherToMaybe <<< read) (getter req)


intlReqGetUrl :: forall e. Request -> ExpressM e String
intlReqGetUrl = unsafeForeignFunction ["req", ""] "req.url"

intlReqGetOriginalUrl :: forall e. Request -> ExpressM e String
intlReqGetOriginalUrl = unsafeForeignFunction ["req", ""] "req.originalUrl"
