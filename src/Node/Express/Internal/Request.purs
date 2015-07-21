-- Think twice before import this into your module, young jedi --
module Node.Express.Internal.Request where

import Prelude
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.EasyFFI
import Node.Express.Internal.Utils
import Node.Express.Types
import Node.Express.Internal.QueryString


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



intlReqAccepts :: forall e. Request -> String -> ExpressM e (Maybe String)
intlReqAccepts req types = do
    let getter = unsafeForeignFunction ["req", "types", ""] "req.accepts(types);"
    liftM1 (eitherToMaybe <<< read) (getter req types)

intlReqAcceptsCharset :: forall e. Request -> String -> ExpressM e (Maybe String)
intlReqAcceptsCharset req charset = do
    let getter = unsafeForeignFunction ["req", "charset", ""] "req.acceptsCharset(charset);"
    liftM1 (eitherToMaybe <<< read) (getter req charset)

intlReqAcceptsLanguage :: forall e. Request -> String -> ExpressM e (Maybe String)
intlReqAcceptsLanguage req language = do
    let getter = unsafeForeignFunction ["req", "language", ""] "req.acceptsLanguage(language);"
    liftM1 (eitherToMaybe <<< read) (getter req language)

intlReqHasType :: forall e. Request -> String -> ExpressM e Boolean
intlReqHasType req type_ = do
    let getter = unsafeForeignFunction ["req", "type", ""] "req.is(type);"
    val <- liftM1 (eitherToMaybe <<< read) (getter req type_)
    return $ fromMaybe false val


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
