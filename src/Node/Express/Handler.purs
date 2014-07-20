module Node.Express.Handler
    ( HandlerM()
    , Handler()
    , withHandler, next
    , params, param
    , getCookie, getSignedCookie
    , getRequestHeader
    , status
    , getResponseHeader, setResponseHeader, setContentType
    , setCookie, clearCookie
    , send, json, jsonp
    , redirect, location
    ) where


import Data.Maybe
import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Node.Express.Types
import Node.Express.Internal.Response
import Node.Express.Internal.Request


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

params :: forall a. (RequestParam a) => a -> HandlerM (Maybe String)
params name = HandlerM \req _ _ ->
    intlReqParams req name

param :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
param name = HandlerM \req _ _ ->
    intlReqParam req name

getCookie :: String -> HandlerM (Maybe String)
getCookie name = HandlerM \req _ _ ->
    intlReqGetCookie req name

getSignedCookie :: String -> HandlerM (Maybe String)
getSignedCookie name = HandlerM \req _ _ ->
    intlReqGetSignedCookie req name

getRequestHeader :: forall a. (ReadForeign a) => String -> HandlerM (Maybe a)
getRequestHeader field = HandlerM \req _ _ ->
    intlReqGetHeader req field

-- Response --

status :: Number -> Handler
status val = HandlerM \_ resp _ ->
    intlRespStatus resp val

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

json :: forall a. a -> Handler
json data_ = HandlerM \_ resp _ ->
    intlRespJson resp data_

jsonp :: forall a. a -> Handler
jsonp data_ = HandlerM \_ resp _ ->
    intlRespJsonp resp data_

redirect :: String -> Handler
redirect url = HandlerM \_ resp _ ->
    intlRespRedirect resp url

location :: String -> Handler
location url = HandlerM \_ resp _ ->
    intlRespLocation resp url

setContentType :: String -> Handler
setContentType t = HandlerM \_ resp _ -> intlRespType resp t
