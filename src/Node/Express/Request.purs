module Node.Express.Request
    ( getRouteParam, getQueryParam, getQueryParams, getBody
    , getBodyParam, getRoute
    , getCookie, getSignedCookie
    , getRequestHeader
    , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
    , getRemoteIp, getRemoteIps, getPath, getHostname, getSubdomains
    , isFresh, isStale
    , isXhr, getProtocol, getMethod
    , getUrl, getOriginalUrl
    , getUserData, setUserData
    ) where

import Prelude
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class IsForeign, read)
import Data.Function.Uncurried (Fn2(), Fn3(), runFn2, runFn3)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (class RequestParam, ExpressM, Request, EXPRESS, 
                           Method, Protocol)
import Node.Express.Internal.Utils (eitherToMaybe)
import Node.Express.Internal.QueryString (Param, parse, getAll, getOne)

-- | Get route param value. If it is named route, e.g `/user/:id` then
-- | `getRouteParam "id"` return matched part of route. If it is
-- | regex route, e.g. `/user/(\d+)` then `getRouteParam 1` return
-- | part that matched `(\d+)` and `getRouteParam 0` return whole
-- | route.
getRouteParam :: forall e a. (RequestParam a) => a -> HandlerM (express :: EXPRESS | e) (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getRouteParam req name)

-- | Get the request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBody :: forall e a. (IsForeign a) => HandlerM (express :: EXPRESS | e) (Either MultipleErrors a)
getBody = HandlerM \req _ _ ->
    liftEff $ liftM1 (runExcept <<< read) (_getBody req)

-- | Get param from request's body.
-- | NOTE: Not parsed by default, you must attach proper middleware
-- |       See http://expressjs.com/4x/api.html#req.body
getBodyParam :: forall e a. (IsForeign a) => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
getBodyParam name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getBodyParam req name)

-- | Get param from query string (part of URL behind '?').
-- | If there are multiple params having equal keys
-- | return the first one.
getQueryParam :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
getQueryParam name = HandlerM \req _ _ -> do
    params <- liftEff $ queryParams req
    pure $ getOne params name

-- | Get all params from query string having specified key.
getQueryParams :: forall e. String -> HandlerM (express :: EXPRESS | e) (Array String)
getQueryParams name = HandlerM \req _ _ -> do
    params <- liftEff $ queryParams req
    pure $ getAll params name

-- | Return route that matched this request.
getRoute :: forall e. HandlerM (express :: EXPRESS | e) String
getRoute = HandlerM \req _ _ ->
    liftEff $ _getRoute req

-- | Get cookie param by its key.
getCookie :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
getCookie name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getCookie req name)

-- | Get signed cookie param by its key.
getSignedCookie :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getSignedCookie req name)
                                   
-- | Get request header param.
getRequestHeader :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
getRequestHeader field = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getHeader req field)

-- | Check if specified response type will be accepted by a client.
accepts :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
accepts types = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _accepts req types)

-- | Execute specified handler if client accepts specified response type.
ifAccepts :: forall e. String -> Handler e -> Handler e
ifAccepts type_ act = do
    isAccepted <- liftM1 (maybe false (const true)) $ accepts type_
    when isAccepted act

-- | Check if specified charset is accepted.
acceptsCharset :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _acceptsCharset req charset)

-- | Check if specified language is accepted.
acceptsLanguage :: forall e. String -> HandlerM (express :: EXPRESS | e) (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _acceptsLanguage req language)

-- | Check if request's Content-Type field matches type.
-- | See http://expressjs.com/4x/api.html#req.is
hasType :: forall e. String -> HandlerM (express :: EXPRESS | e) Boolean
hasType type_ = HandlerM \req _ _ -> do
    val <- liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _hasType req type_)
    pure $ fromMaybe false val

-- | Return remote or upstream address.
getRemoteIp :: forall e. HandlerM (express :: EXPRESS | e) String
getRemoteIp = HandlerM \req _ _ ->
    liftEff $ _getRemoteIp req

-- | Return list of X-Forwarded-For proxies if any.
getRemoteIps :: forall e. HandlerM (express :: EXPRESS | e) (Array String)
getRemoteIps = HandlerM \req _ _ ->
    liftEff $ _getRemoteIps req

-- | Return request URL pathname.
getPath :: forall e. HandlerM (express :: EXPRESS | e) String
getPath = HandlerM \req _ _ ->
    liftEff $ _getPath req

-- | Return Host header field.
getHostname :: forall e. HandlerM (express :: EXPRESS | e) String
getHostname = HandlerM \req _ _ ->
    liftEff $ _getHostname req

-- | Return array of subdomains.
getSubdomains :: forall e. HandlerM (express :: EXPRESS | e) (Array String)
getSubdomains = HandlerM \req _ _ ->
    liftEff $ _getSubdomains req

-- | Check that Last-Modified and/or ETag still matches.
isFresh :: forall e. HandlerM (express :: EXPRESS | e) Boolean
isFresh = HandlerM \req _ _ ->
    liftEff $ _isFresh req

-- | Check that Last-Modified and/or ETag do not match.
isStale :: forall e. HandlerM (express :: EXPRESS | e) Boolean
isStale = HandlerM \req _ _ ->
    liftEff $ _isStale req

-- | Check if request was issued by XMLHttpRequest.
isXhr :: forall e. HandlerM (express :: EXPRESS | e) Boolean
isXhr = HandlerM \req _ _ ->
    liftEff $ _isXhr req

-- | Return request protocol.
getProtocol :: forall e. HandlerM (express :: EXPRESS | e) (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (_getProtocol req)

-- | Return request HTTP method
getMethod :: forall e. HandlerM (express :: EXPRESS | e) (Maybe Method)
getMethod = HandlerM \req _ _ ->
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (_getMethod req)

-- | Return request URL (may be modified by other handlers/middleware).
getUrl :: forall e. HandlerM (express :: EXPRESS | e) String
getUrl = HandlerM \req _ _ ->
    liftEff $ _getUrl req

-- | Return request original URL.
getOriginalUrl :: forall e. HandlerM (express :: EXPRESS | e) String
getOriginalUrl = HandlerM \req _ _ ->
    liftEff $ _getOriginalUrl req

-- | Sets the specified field of the userData object attached to the Request 
-- | object to specified data
setUserData :: forall a e. String -> a -> Handler e
setUserData field val = HandlerM \req _ _ ->
    liftEff $ runFn3 _setData req field val

-- | Retrieves the data from the request set with previous call to `setUserData`
getUserData :: forall e a. (IsForeign a) => String -> 
                           HandlerM (express :: EXPRESS | e) (Maybe a)
getUserData field = HandlerM \req _ _ -> do
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getData req field)

queryParams :: forall e. Request -> ExpressM e (Array Param)
queryParams req = do
    query <- _getQueryParams req
    case parse query of
        Left _ -> pure []
        Right params -> pure params

foreign import _getRouteParam :: forall e a. Fn2 Request a (ExpressM e Foreign)

foreign import _getRoute :: forall e. Request -> ExpressM e String

foreign import _getBody :: forall e. Request -> ExpressM e Foreign

foreign import _getBodyParam :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getQueryParams :: forall e. Request -> ExpressM e String

foreign import _getCookie :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getSignedCookie :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getHeader :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _accepts :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _acceptsCharset :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _acceptsLanguage :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _hasType :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getRemoteIp :: forall e. Request -> ExpressM e String

foreign import _getRemoteIps :: forall e. Request -> ExpressM e (Array String)

foreign import _getPath :: forall e. Request -> ExpressM e String

foreign import _getHostname :: forall e. Request -> ExpressM e String

foreign import _getSubdomains :: forall e. Request -> ExpressM e (Array String)

foreign import _isFresh :: forall e. Request -> ExpressM e Boolean

foreign import _isStale :: forall e. Request -> ExpressM e Boolean

foreign import _isXhr :: forall e. Request -> ExpressM e Boolean

foreign import _getProtocol :: forall e. Request -> ExpressM e Foreign

foreign import _getMethod :: forall e. Request -> ExpressM e Foreign

foreign import _getUrl :: forall e. Request -> ExpressM e String

foreign import _getOriginalUrl :: forall e. Request -> ExpressM e String

foreign import _setData :: forall a e. Fn3 Request String a (ExpressM e Unit)

foreign import _getData :: forall e. Fn2 Request String (ExpressM e Foreign)
