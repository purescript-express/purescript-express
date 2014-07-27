module Node.Express.Internal.Request where

import Data.Maybe
import Data.Foreign
import Data.Foreign.EasyFFI
import Node.Express.Internal.Utils
import Node.Express.Types


intlReqRouteParam ::
    forall a. (RequestParam a) =>
    Request -> a -> ExpressM (Maybe String)
intlReqRouteParam req name = do
    let getter :: forall a. Request -> a -> ExpressM Foreign
        getter = unsafeForeignFunction ["req", "name", ""] "req.params[name]"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req name)

intlReqParam ::
    forall a. (ReadForeign a) =>
    Request -> String -> ExpressM (Maybe a)
intlReqParam req name = do
    let getter = unsafeForeignFunction ["req", "name", ""] "req.param(name)"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req name)

intlReqRoute ::
    Request -> ExpressM String
intlReqRoute = unsafeForeignFunction ["req", ""] "req.route.path"


intlReqGetCookie ::
    Request -> String -> ExpressM (Maybe String)
intlReqGetCookie req name = do
    let getter = unsafeForeignFunction ["req", "name", ""] "req.cookies[name]"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req name)

intlReqGetSignedCookie ::
    Request -> String -> ExpressM (Maybe String)
intlReqGetSignedCookie req name = do
    let getter = unsafeForeignFunction ["req", "name", ""] "req.signedCookies[name]"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req name)


intlReqGetHeader ::
    forall a. (ReadForeign a) =>
    Request -> String -> ExpressM (Maybe a)
intlReqGetHeader req field = do
    let getter = unsafeForeignFunction ["req", "field", ""] "req.get(field);"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req field)


intlReqAccepts :: Request -> String -> ExpressM (Maybe String)
intlReqAccepts req types = do
    let getter = unsafeForeignFunction ["req", "types", ""] "req.accepts(types);"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req types)

intlReqAcceptsCharset :: Request -> String -> ExpressM (Maybe String)
intlReqAcceptsCharset req charset = do
    let getter = unsafeForeignFunction ["req", "charset", ""] "req.acceptsCharset(charset);"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req charset)

intlReqAcceptsLanguage :: Request -> String -> ExpressM (Maybe String)
intlReqAcceptsLanguage req language = do
    let getter = unsafeForeignFunction ["req", "language", ""] "req.acceptsLanguage(language);"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req language)

intlReqHasType :: Request -> String -> ExpressM Boolean
intlReqHasType req type_ = do
    let getter = unsafeForeignFunction ["req", "type", ""] "req.is(type);"
    val <- liftM1 (eitherToMaybe <<< parseForeign read) (getter req type_)
    return $ fromMaybe false val


intlReqGetRemoteIp :: Request -> ExpressM String
intlReqGetRemoteIp = unsafeForeignFunction ["req", ""] "req.ip"

intlReqGetRemoteIps :: Request -> ExpressM [String]
intlReqGetRemoteIps = unsafeForeignFunction ["req", ""] "req.ips"

intlReqGetPath :: Request -> ExpressM String
intlReqGetPath = unsafeForeignFunction ["req", ""] "req.path"

intlReqGetHost :: Request -> ExpressM String
intlReqGetHost = unsafeForeignFunction ["req", ""] "req.host"

intlReqGetSubdomains :: Request -> ExpressM [String]
intlReqGetSubdomains = unsafeForeignFunction ["req", ""] "req.subdomains"


intlReqIsFresh :: Request -> ExpressM Boolean
intlReqIsFresh = unsafeForeignFunction ["req", ""] "req.fresh"

intlReqIsStale :: Request -> ExpressM Boolean
intlReqIsStale = unsafeForeignFunction ["req", ""] "req.stale"


intlReqIsXhr :: Request -> ExpressM Boolean
intlReqIsXhr = unsafeForeignFunction ["req", ""] "req.xhr"

intlReqGetProtocol :: Request -> ExpressM (Maybe Protocol)
intlReqGetProtocol req = do
    let getter = unsafeForeignFunction ["req", ""] "req.protocol"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req)


intlReqGetUrl :: Request -> ExpressM String
intlReqGetUrl = unsafeForeignFunction ["req", ""] "req.url"

intlReqGetOriginalUrl :: Request -> ExpressM String
intlReqGetOriginalUrl = unsafeForeignFunction ["req", ""] "req.originalUrl"


intlReqPutUserData ::
    forall a. (ReadForeign a) =>
    Request -> String -> a -> ExpressM Unit
intlReqPutUserData = unsafeForeignProcedure ["req", "key", "data", ""]
    "if (req.userData === undefined) { \
    \    req.userData = {}; \
    \} \
    \req.userData[key] = data;"

intlReqGetUserData ::
    forall a. (ReadForeign a) =>
    Request -> String -> ExpressM (Maybe a)
intlReqGetUserData req key = do
    let getter = unsafeForeignProcedure ["req", "key", ""]
            "if (req.userData === undefined) { \
            \   return undefined; \
            \} \
            \return req.userData[key];"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req key)


-- TODO: query!!
