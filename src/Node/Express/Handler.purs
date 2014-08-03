module Node.Express.Handler
    ( HandlerM()
    , Handler()
    , withHandler, next
    -- Request
    , getRouteParam, getParam, getQueryParam, getQueryParams
    , getRoute
    , getCookie, getSignedCookie
    , getRequestHeader
    , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
    , getRemoteIp, getRemoteIps, getPath, getHost, getSubdomains
    , isFresh, isStale
    , isXhr, getProtocol
    , getUrl, getOriginalUrl
    , putUserData, getUserData
    -- Response
    , setStatus
    , getResponseHeader, setResponseHeader, setContentType
    , setCookie, clearCookie
    , send, sendJson, sendJsonp
    , redirect, setLocation
    , sendFile, sendFileExt, download, downloadExt
    ) where


import Data.Maybe
import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad
import Node.Express.Types
import Node.Express.Internal.Response
import Node.Express.Internal.Request
import Node.Express.Internal.QueryString


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

instance monadEffHandlerM :: MonadEff HandlerM where
    liftEff act = HandlerM \_ _ _ -> liftEff act

withHandler :: Handler -> Request -> Response -> ExpressM Unit -> ExpressM Unit
withHandler (HandlerM h) = h

next :: Handler
next = HandlerM \_ _ nxt -> nxt

-- Request --

getRouteParam :: forall a. (RequestParam a) => a -> HandlerM (Maybe String)
getRouteParam name = HandlerM \req _ _ ->
    intlReqRouteParam req name

getParam :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
getParam name = HandlerM \req _ _ ->
    intlReqParam req name

getQueryParam :: String -> HandlerM (Maybe String)
getQueryParam name = HandlerM \req _ _ -> do
    params <- intlReqQueryParams req
    return $ getOne params name

getQueryParams :: String -> HandlerM [String]
getQueryParams name = HandlerM \req _ _ -> do
    params <- intlReqQueryParams req
    return $ getAll params name

getRoute :: HandlerM String
getRoute = HandlerM \req _ _ ->
    intlReqRoute req

getCookie :: String -> HandlerM (Maybe String)
getCookie name = HandlerM \req _ _ ->
    intlReqGetCookie req name

getSignedCookie :: String -> HandlerM (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    intlReqGetSignedCookie req name

getRequestHeader :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
getRequestHeader field = HandlerM \req _ _ ->
    intlReqGetHeader req field

accepts :: String -> HandlerM (Maybe String)
accepts types = HandlerM \req _ _ ->
    intlReqAccepts req types

ifAccepts :: String -> Handler -> Handler
ifAccepts type_ act = do
    isAccepted <- (liftM1 (maybe false (const true)) $ accepts type_)
    when isAccepted act

acceptsCharset :: String -> HandlerM (Maybe String)
acceptsCharset charset = HandlerM \req _ _ ->
    intlReqAcceptsCharset req charset

acceptsLanguage :: String -> HandlerM (Maybe String)
acceptsLanguage language = HandlerM \req _ _ ->
    intlReqAcceptsLanguage req language

hasType :: String -> HandlerM Boolean
hasType type_ = HandlerM \req _ _ ->
    intlReqHasType req type_

getRemoteIp :: HandlerM String
getRemoteIp = HandlerM \req _ _ ->
    intlReqGetRemoteIp req

getRemoteIps :: HandlerM [String]
getRemoteIps = HandlerM \req _ _ ->
    intlReqGetRemoteIps req

getPath :: HandlerM String
getPath = HandlerM \req _ _ ->
    intlReqGetPath req

getHost :: HandlerM String
getHost = HandlerM \req _ _ ->
    intlReqGetHost req

getSubdomains :: HandlerM [String]
getSubdomains = HandlerM \req _ _ ->
    intlReqGetSubdomains req

isFresh :: HandlerM Boolean
isFresh = HandlerM \req _ _ ->
    intlReqIsFresh req

isStale :: HandlerM Boolean
isStale = HandlerM \req _ _ ->
    intlReqIsStale req

isXhr :: HandlerM Boolean
isXhr = HandlerM \req _ _ ->
    intlReqIsXhr req

getProtocol :: HandlerM (Maybe Protocol)
getProtocol = HandlerM \req _ _ ->
    intlReqGetProtocol req

getUrl :: HandlerM String
getUrl = HandlerM \req _ _ ->
    intlReqGetUrl req

getOriginalUrl :: HandlerM String
getOriginalUrl = HandlerM \req _ _ ->
    intlReqGetOriginalUrl req

putUserData :: forall a. (ReadForeign a) => String -> a -> Handler
putUserData key val = HandlerM \req _ _ ->
    intlReqPutUserData req key val

getUserData :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
getUserData key = HandlerM \req _ _ ->
    intlReqGetUserData req key

-- Response --

setStatus :: Number -> Handler
setStatus val = HandlerM \_ resp _ ->
    intlRespSetStatus resp val

getResponseHeader :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
getResponseHeader field = HandlerM \_ resp _ -> do
    intlRespGetHeader resp field

setResponseHeader :: forall a. String -> a -> Handler
setResponseHeader field val = HandlerM \_ resp _ ->
    intlRespSetHeader resp field val

setCookie :: forall o. String -> String -> { | o } -> Handler
setCookie name val opts = HandlerM \_ resp _ ->
    intlRespSetCookie resp name val opts

clearCookie :: forall o. String -> { | o } -> Handler
clearCookie name opts = HandlerM \_ resp _ ->
    intlRespClearCookie resp name opts

send :: forall a. a -> Handler
send data_ = HandlerM \_ resp _ ->
    intlRespSend resp data_

sendJson :: forall a. a -> Handler
sendJson data_ = HandlerM \_ resp _ ->
    intlRespSendJson resp data_

sendJsonp :: forall a. a -> Handler
sendJsonp data_ = HandlerM \_ resp _ ->
    intlRespSendJsonp resp data_

redirect :: String -> Handler
redirect url = HandlerM \_ resp _ ->
    intlRespRedirect resp url

setLocation :: String -> Handler
setLocation url = HandlerM \_ resp _ ->
    intlRespSetLocation resp url

setContentType :: String -> Handler
setContentType t = HandlerM \_ resp _ ->
    intlRespType resp t

sendFile :: String -> Handler
sendFile path = HandlerM \_ resp _ ->
    intlRespSendFile resp path {} (\_ -> return unit)

sendFileExt :: forall o. String -> { | o } -> (Error -> ExpressM Unit) -> Handler
sendFileExt path opts callback = HandlerM \_ resp _ ->
    intlRespSendFile resp path opts callback

download :: String -> Handler
download path = HandlerM \_ resp _ ->
    intlRespDownload resp path "" (\_ -> return unit)

downloadExt :: String -> String -> (Error -> ExpressM Unit) -> Handler
downloadExt path filename callback = HandlerM \_ resp _ ->
    intlRespDownload resp path filename callback
