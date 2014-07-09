module Node.Express.Handler
    ( HandlerM()
    , Handler()
    , withHandler
    , liftExpress
    , status
    , getHeader, setHeader, setContentType
    , setCookie, clearCookie
    , send, json, jsonp
    , redirect, location
    ) where


import Data.Either
import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Trans
import Node.Express.Types
import Node.Express.Internal.Response


data HandlerM a = HandlerM (Request -> Response -> ExpressM a)
type Handler = HandlerM Unit


instance functorHandlerM :: Functor HandlerM where
    (<$>) f (HandlerM h) = HandlerM \req resp ->
        (h req resp >>= \r -> return $ f r)

instance applyHandlerM :: Apply HandlerM where
    (<*>) (HandlerM f) (HandlerM h) = HandlerM \req resp -> do
        res   <- h req resp
        trans <- f req resp
        return $ trans res

instance applicativeHandlerM :: Applicative HandlerM where
    pure x = HandlerM \_ _ -> return x

instance bindHandlerM :: Bind HandlerM where
    (>>=) (HandlerM h) f = HandlerM \req resp -> do
        (HandlerM g) <- liftM1 f $ h req resp
        g req resp

instance monadHandlerM :: Monad HandlerM

instance monadEffHandlerM :: MonadEff HandlerM where
    liftEff act = HandlerM \_ _ -> liftEff act

withHandler :: Handler -> Request -> Response -> ExpressM Unit
withHandler (HandlerM h) = h

liftExpress :: forall a. ExpressM a -> HandlerM a
liftExpress act = HandlerM \_ _ -> act

-- Request --


-- Response --

status :: Number -> Handler
status val = HandlerM \_ resp ->
    intlRespStatus resp val

getHeader :: forall a. (ReadForeign a) => String -> HandlerM (Either String a)
getHeader field = HandlerM \_ resp ->
    intlRespGetHeader resp field

setHeader :: forall a. String -> a -> Handler
setHeader field val = HandlerM \_ resp ->
    intlRespSetHeader resp field val

setCookie :: forall o. String -> String -> { | o } -> Handler
setCookie name val opts = HandlerM \_ resp ->
    intlRespSetCookie resp name val opts

clearCookie :: forall o. String -> { | o } -> Handler
clearCookie name opts = HandlerM \_ resp ->
    intlRespClearCookie resp name opts

send :: forall a. a -> Handler
send data_ = HandlerM \_ resp -> intlRespSend resp data_

json :: forall a. a -> Handler
json data_ = HandlerM \_ resp -> intlRespJson resp data_

jsonp :: forall a. a -> Handler
jsonp data_ = HandlerM \_ resp -> intlRespJsonp resp data_

redirect :: String -> Handler
redirect url = HandlerM \_ resp -> intlRespRedirect resp url

location :: String -> Handler
location url = HandlerM \_ resp -> intlRespLocation resp url

setContentType :: String -> Handler
setContentType t = HandlerM \_ resp -> intlRespType resp t
