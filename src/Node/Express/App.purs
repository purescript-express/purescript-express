module Node.Express.App
    ( AppM()
    , App()
    , listen, use, useAt, useOnParam
    , getProp, setProp
    , http, get, post, put, delete, all
    ) where

import Data.Foreign
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Class

import Node.Express.Types
import Node.Express.Internal.App
import Node.Express.Handler


data AppM a = AppM (Application -> ExpressM a)
type App = AppM Unit

instance functorAppM :: Functor AppM where
    (<$>) f (AppM h) = AppM \app -> liftM1 f $ h app

instance applyAppM :: Apply AppM where
    (<*>) (AppM f) (AppM h) = AppM \app -> do
        res <- h app
        trans <- f app
        return $ trans res

instance applicativeAppM :: Applicative AppM where
    pure x = AppM \_ -> return x

instance bindAppM :: Bind AppM where
    (>>=) (AppM h) f = AppM \app -> do
        res <- h app
        case f res of
             AppM g -> g app

instance monadAppM :: Monad AppM

instance monadEffAppM :: MonadEff AppM where
    liftEff act = AppM \_ -> liftEff act


listen :: forall e. App -> Port -> (Event -> Eff e Unit) -> ExpressM Unit
listen (AppM act) port cb = do
    app <- intlMkApplication
    act app
    intlAppListen app port cb

use :: Handler -> App
use middleware = AppM \app ->
    intlAppUse app (\req resp nxt -> withHandler middleware req resp nxt)

useAt :: Path -> Handler -> App
useAt route middleware = AppM \app ->
    intlAppUseAt app route (\req resp nxt -> withHandler middleware req resp nxt)

useOnParam :: String -> (String -> Handler) -> App
useOnParam param handler = AppM \app ->
    intlAppUseOnParam app param
        (\val req resp nxt -> withHandler (handler val) req resp nxt)


getProp :: forall a. (ReadForeign a) => String -> AppM (Maybe a)
getProp name = AppM \app ->
    intlAppGetProp app name

setProp :: forall a. (ReadForeign a) => String -> a -> App
setProp name val = AppM \app ->
    intlAppSetProp app name val


http :: forall r. (Route r) => Method -> r -> Handler -> App
http method route handler = AppM \app ->
    intlAppHttp app (show method) route $ withHandler handler

get :: forall r. (Route r) => r -> Handler -> App
get = http GET

post :: forall r. (Route r) => r -> Handler -> App
post = http POST

put :: forall r. (Route r) => r -> Handler -> App
put = http PUT

delete :: forall r. (Route r) => r -> Handler -> App
delete = http DELETE

all :: forall r. (Route r) => r -> Handler -> App
all = http ALL

