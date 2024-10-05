module Node.Express.Request
  ( getRouteParam
  , getRouteParams
  , getQueryParam
  , getQueryParams
  , getBody
  , getBody'
  , getBodyParam
  , getRoute
  , getCookie
  , getSignedCookie
  , getRequestHeader
  , getRequestHeaders
  , accepts
  , ifAccepts
  , acceptsCharset
  , acceptsLanguage
  , hasType
  , getRemoteIp
  , getRemoteIps
  , getPath
  , getHostname
  , getSubdomains
  , isFresh
  , isStale
  , isXhr
  , getProtocol
  , getMethod
  , getUrl
  , getOriginalUrl
  , getUserData
  , setUserData
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Aff.Compat (EffectFn3)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (F, Foreign)
import Foreign.Generic.Class (class Decode, decode)
import Foreign.Object (Object)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (class RequestParam, Request, Method, Protocol, decodeProtocol, decodeMethod)

-- | Get route param value. If it is named route, e.g `/user/:id` then
-- | `getRouteParam "id"` return matched part of route. If it is
-- | regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
-- | part that matched `(\d+)` and `getRouteParam 0` return whole
-- | route.
getRouteParam :: forall a. RequestParam a => a -> HandlerM (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getRouteParam req name

-- | Get all route params.
getRouteParams :: HandlerM (Object Foreign)
getRouteParams = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getRouteParams req

-- | Get the request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBody :: forall a. Decode a => HandlerM (F a)
getBody = HandlerM \req _ _ ->
  liftEffect $ liftM1 decode (runEffectFn1 _getBody req)

-- | Get the request's body without a `Decode` parsing.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBody' :: HandlerM Foreign
getBody' = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getBody req

-- | Get param from request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall a. String -> HandlerM (Maybe a)
getBodyParam name = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getBodyParam req name

-- | Get param from query string (part of URL behind '?').
-- | It could be any JS object, e.g. an array in case multiple repeating query
-- | parameters, or an object in case of nested query parameters (this
-- | particular behavior depends on the type of query parser - 'simple' won't
-- | parse complex objects, see
-- | https://github.com/expressjs/express/blob/master/test/req.query.js)
getQueryParam :: forall a. String -> HandlerM (Maybe a)
getQueryParam name = HandlerM \req _ _ -> do
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getQueryParam req name

-- | Shortcut for `getQueryParam paramName :: HandlerM (Maybe (Array a))`
getQueryParams :: forall a. String -> HandlerM (Maybe (Array a))
getQueryParams = getQueryParam

-- | Return route that matched this request.
getRoute :: HandlerM String
getRoute = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getRoute req

-- | Get cookie param by its key.
getCookie :: String -> HandlerM (Maybe String)
getCookie name = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getCookie req name

-- | Get signed cookie param by its key.
getSignedCookie :: String -> HandlerM (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getSignedCookie req name

-- | Get request header param.
getRequestHeader :: String -> HandlerM (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getHeader req field

-- | Get all request headers.
getRequestHeaders :: HandlerM (Object Foreign)
getRequestHeaders = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _getHeaders req

-- | Check if specified response type will be accepted by a client.
accepts :: String -> HandlerM (Maybe String)
accepts types = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _accepts req types

-- | Execute specified handler if client accepts specified response type.
ifAccepts :: String -> Handler -> Handler
ifAccepts type_ act = do
  isAccepted <- liftM1 (maybe false (const true)) $ accepts type_
  when isAccepted act

-- | Check if specified charset is accepted.
acceptsCharset :: String -> HandlerM (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _acceptsCharset req charset

-- | Check if specified language is accepted.
acceptsLanguage :: String -> HandlerM (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _acceptsLanguage req language

-- | Check if request's Content-Type field matches type.
-- | See http://expressjs.com/4x/api.html#req.is
hasType :: String -> HandlerM Boolean
hasType type_ = HandlerM \req _ _ ->
  liftEffect $ runEffectFn2 _hasType req type_

-- | Return remote or upstream address.
getRemoteIp :: HandlerM String
getRemoteIp = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getRemoteIp req

-- | Return list of X-Forwarded-For proxies if any.
getRemoteIps :: HandlerM (Array String)
getRemoteIps = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getRemoteIps req

-- | Return request URL pathname.
getPath :: HandlerM String
getPath = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getPath req

-- | Return Host header field.
getHostname :: HandlerM String
getHostname = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getHostname req

-- | Return array of subdomains.
getSubdomains :: HandlerM (Array String)
getSubdomains = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getSubdomains req

-- | Check that Last-Modified and/or ETag still matches.
isFresh :: HandlerM Boolean
isFresh = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _isFresh req

-- | Check that Last-Modified and/or ETag do not match.
isStale :: HandlerM Boolean
isStale = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _isStale req

-- | Check if request was issued by XMLHttpRequest.
isXhr :: HandlerM Boolean
isXhr = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _isXhr req

-- | Return request protocol.
getProtocol :: HandlerM (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
  liftEffect $ liftM1 decodeProtocol (runEffectFn1 _getProtocol req)

-- | Return request HTTP method
getMethod :: HandlerM Method
getMethod = HandlerM \req _ _ ->
  liftEffect $ liftM1 decodeMethod (runEffectFn1 _getMethod req)

-- | Return request URL (may be modified by other handlers/middleware).
getUrl :: HandlerM String
getUrl = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getUrl req

-- | Return request original URL.
getOriginalUrl :: HandlerM String
getOriginalUrl = HandlerM \req _ _ ->
  liftEffect $ runEffectFn1 _getOriginalUrl req

-- | Sets the specified field of the userData object attached to the Request
-- | object to specified data
setUserData :: forall a. String -> a -> Handler
setUserData field val = HandlerM \req _ _ ->
  liftEffect $ runEffectFn3 _setData req field val

-- | Retrieves the data from the request set with previous call to `setUserData`
getUserData :: forall a. String -> HandlerM (Maybe a)
getUserData field = HandlerM \req _ _ -> do
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getData req field

foreign import _getRouteParam :: forall a. EffectFn2 Request a (Nullable String)
foreign import _getRouteParams :: EffectFn1 Request (Object Foreign)
foreign import _getRoute :: EffectFn1 Request String
foreign import _getBody :: EffectFn1 Request Foreign
foreign import _getBodyParam :: forall a. EffectFn2 Request String (Nullable a)
foreign import _getQueryParam :: forall a. EffectFn2 Request String (Nullable a)
foreign import _getCookie :: EffectFn2 Request String (Nullable String)
foreign import _getSignedCookie :: EffectFn2 Request String (Nullable String)
foreign import _getHeader :: EffectFn2 Request String (Nullable String)
foreign import _getHeaders :: EffectFn1 Request (Object Foreign)
foreign import _accepts :: EffectFn2 Request String (Nullable String)
foreign import _acceptsCharset :: EffectFn2 Request String (Nullable String)
foreign import _acceptsLanguage :: EffectFn2 Request String (Nullable String)
foreign import _hasType :: EffectFn2 Request String Boolean
foreign import _getRemoteIp :: EffectFn1 Request String
foreign import _getRemoteIps :: EffectFn1 Request (Array String)
foreign import _getPath :: EffectFn1 Request String
foreign import _getHostname :: EffectFn1 Request String
foreign import _getSubdomains :: EffectFn1 Request (Array String)
foreign import _isFresh :: EffectFn1 Request Boolean
foreign import _isStale :: EffectFn1 Request Boolean
foreign import _isXhr :: EffectFn1 Request Boolean
foreign import _getProtocol :: EffectFn1 Request String
foreign import _getMethod :: EffectFn1 Request String
foreign import _getUrl :: EffectFn1 Request String
foreign import _getOriginalUrl :: EffectFn1 Request String
foreign import _setData :: forall a. EffectFn3 Request String a Unit
foreign import _getData :: forall a. EffectFn2 Request String (Nullable a)
