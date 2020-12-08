module Node.Express.Request
  ( getRouteParam, getRouteParams, getQueryParam, getQueryParams, getBody, getBody'
  , getBodyParam, getRoute
  , getCookie, getSignedCookie
  , getRequestHeader, getRequestHeaders
  , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
  , getRemoteIp, getRemoteIps, getPath, getHostname, getSubdomains
  , isFresh, isStale
  , isXhr, getProtocol, getMethod
  , getUrl, getOriginalUrl
  , getUserData, setUserData
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (F, Foreign)
import Foreign.Object (Object)
import Foreign.Class (class Decode, decode)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (class RequestParam, Request, Method, Protocol, decodeProtocol, decodeMethod)

-- | Get route param value. If it is named route, e.g `/user/:id` then
-- | `getRouteParam "id"` return matched part of route. If it is
-- | regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
-- | part that matched `(\d+)` and `getRouteParam 0` return whole
-- | route.
getRouteParam :: forall a. RequestParam a => a -> HandlerM (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
    liftEffect $ runFn4 _getRouteParam req name Nothing Just

-- | Get all route params.
getRouteParams :: HandlerM (Object Foreign)
getRouteParams = HandlerM \req _ _ ->
    liftEffect $ _getRouteParams req

-- | Get the request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBody :: forall a. Decode a => HandlerM (F a)
getBody = HandlerM \req _ _ ->
    liftEffect $ liftM1 decode (_getBody req)

-- | Get the request's body without a `Decode` parsing.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBody' :: HandlerM Foreign
getBody' = HandlerM \req _ _ ->
    liftEffect $ _getBody req

-- | Get param from request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall a. String -> HandlerM (Maybe a)
getBodyParam name = HandlerM \req _ _ ->
    liftEffect $ runFn4 _getBodyParam req name Nothing Just

-- | Get param from query string (part of URL behind '?').
-- | It could be any JS object, e.g. an array in case multiple repeating query
-- | parameters, or an object in case of nested query parameters (this
-- | particular behavior depends on the type of query parser - 'simple' won't
-- | parse complex objects, see
-- | https://github.com/expressjs/express/blob/master/test/req.query.js)
getQueryParam :: forall a. String -> HandlerM (Maybe a)
getQueryParam name = HandlerM \req _ _ -> do
    liftEffect $ runFn4 _getQueryParam req name Nothing Just

-- | Shortcut for `getQueryParam paramName :: HandlerM (Maybe (Array a))`
getQueryParams :: forall a. String -> HandlerM (Maybe (Array a))
getQueryParams = getQueryParam

-- | Return route that matched this request.
getRoute :: HandlerM String
getRoute = HandlerM \req _ _ ->
    liftEffect $ _getRoute req

-- | Get cookie param by its key.
getCookie :: String -> HandlerM (Maybe String)
getCookie name = HandlerM \req _ _ ->
    liftEffect $ runFn4 _getCookie req name Nothing Just

-- | Get signed cookie param by its key.
getSignedCookie :: String -> HandlerM (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    liftEffect $ runFn4 _getSignedCookie req name Nothing Just

-- | Get request header param.
getRequestHeader :: String -> HandlerM (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
    liftEffect $ runFn4 _getHeader req field Nothing Just

-- | Get all request headers.
getRequestHeaders :: HandlerM (Object Foreign)
getRequestHeaders = HandlerM \req _ _ -> liftEffect $ _getHeaders req

-- | Check if specified response type will be accepted by a client.
accepts :: String -> HandlerM (Maybe String)
accepts types = HandlerM \req _ _ ->
    liftEffect $ runFn4 _accepts req types Nothing Just

-- | Execute specified handler if client accepts specified response type.
ifAccepts :: String -> Handler -> Handler
ifAccepts type_ act = do
    isAccepted <- liftM1 (maybe false (const true)) $ accepts type_
    when isAccepted act

-- | Check if specified charset is accepted.
acceptsCharset :: String -> HandlerM (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    liftEffect $ runFn4 _acceptsCharset req charset Nothing Just

-- | Check if specified language is accepted.
acceptsLanguage :: String -> HandlerM (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    liftEffect $ runFn4 _acceptsLanguage req language Nothing Just

-- | Check if request's Content-Type field matches type.
-- | See http://expressjs.com/4x/api.html#req.is
hasType :: String -> HandlerM Boolean
hasType type_ = HandlerM \req _ _ ->
    liftEffect $ runFn2 _hasType req type_

-- | Return remote or upstream address.
getRemoteIp :: HandlerM String
getRemoteIp = HandlerM \req _ _ ->
    liftEffect $ _getRemoteIp req

-- | Return list of X-Forwarded-For proxies if any.
getRemoteIps :: HandlerM (Array String)
getRemoteIps = HandlerM \req _ _ ->
    liftEffect $ _getRemoteIps req

-- | Return request URL pathname.
getPath :: HandlerM String
getPath = HandlerM \req _ _ ->
    liftEffect $ _getPath req

-- | Return Host header field.
getHostname :: HandlerM String
getHostname = HandlerM \req _ _ ->
    liftEffect $ _getHostname req

-- | Return array of subdomains.
getSubdomains :: HandlerM (Array String)
getSubdomains = HandlerM \req _ _ ->
    liftEffect $ _getSubdomains req

-- | Check that Last-Modified and/or ETag still matches.
isFresh :: HandlerM Boolean
isFresh = HandlerM \req _ _ ->
    liftEffect $ _isFresh req

-- | Check that Last-Modified and/or ETag do not match.
isStale :: HandlerM Boolean
isStale = HandlerM \req _ _ ->
    liftEffect $ _isStale req

-- | Check if request was issued by XMLHttpRequest.
isXhr :: HandlerM Boolean
isXhr = HandlerM \req _ _ ->
    liftEffect $ _isXhr req

-- | Return request protocol.
getProtocol :: HandlerM (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
    liftEffect $ liftM1 decodeProtocol (_getProtocol req)

-- | Return request HTTP method
getMethod :: HandlerM Method
getMethod = HandlerM \req _ _ ->
    liftEffect $ liftM1 decodeMethod (_getMethod req)

-- | Return request URL (may be modified by other handlers/middleware).
getUrl :: HandlerM String
getUrl = HandlerM \req _ _ ->
    liftEffect $ _getUrl req

-- | Return request original URL.
getOriginalUrl :: HandlerM String
getOriginalUrl = HandlerM \req _ _ ->
    liftEffect $ _getOriginalUrl req

-- | Sets the specified field of the userData object attached to the Request
-- | object to specified data
setUserData :: forall a. String -> a -> Handler
setUserData field val = HandlerM \req _ _ ->
    liftEffect $ runFn3 _setData req field val

-- | Retrieves the data from the request set with previous call to `setUserData`
getUserData :: forall a. String -> HandlerM (Maybe a)
getUserData field = HandlerM \req _ _ -> do
    liftEffect $ runFn4 _getData req field Nothing Just

foreign import _getRouteParam :: forall a. Fn4 Request a (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _getRouteParams :: Request -> Effect (Object Foreign)

foreign import _getRoute :: Request -> Effect String

foreign import _getBody :: Request -> Effect Foreign

foreign import _getBodyParam :: forall a. Fn4 Request String (Maybe a) (a -> Maybe a) (Effect (Maybe a))

foreign import _getQueryParam :: forall a. Fn4 Request String (Maybe a) (a -> Maybe a) (Effect (Maybe a))

foreign import _getCookie :: Fn4 Request String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _getSignedCookie :: Fn4 Request String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _getHeader :: Fn4 Request String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _getHeaders :: Request -> Effect (Object Foreign)

foreign import _accepts :: Fn4 Request String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _acceptsCharset :: Fn4 Request String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _acceptsLanguage :: Fn4 Request String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _hasType :: Fn2 Request String (Effect Boolean)

foreign import _getRemoteIp :: Request -> Effect String

foreign import _getRemoteIps :: Request -> Effect (Array String)

foreign import _getPath :: Request -> Effect String

foreign import _getHostname :: Request -> Effect String

foreign import _getSubdomains :: Request -> Effect (Array String)

foreign import _isFresh :: Request -> Effect Boolean

foreign import _isStale :: Request -> Effect Boolean

foreign import _isXhr :: Request -> Effect Boolean

foreign import _getProtocol :: Request -> Effect String

foreign import _getMethod :: Request -> Effect String

foreign import _getUrl :: Request -> Effect String

foreign import _getOriginalUrl :: Request -> Effect String

foreign import _setData :: forall a. Fn3 Request String a (Effect Unit)

foreign import _getData :: forall a. Fn4 Request String (Maybe a) (a -> Maybe a) (Effect (Maybe a))
