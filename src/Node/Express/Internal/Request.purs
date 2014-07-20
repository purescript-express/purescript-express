module Node.Express.Internal.Request where

import Data.Maybe
import Data.Foreign
import Data.Foreign.EasyFFI
import Node.Express.Internal.Utils
import Node.Express.Types


class RequestParam a
instance requestParamString :: RequestParam String
instance requestParamNumber :: RequestParam Number

intlReqParams ::
    forall a. (RequestParam a) =>
    Request -> a -> ExpressM (Maybe String)
intlReqParams req name = do
    let getter :: forall a. Request -> a -> ExpressM Foreign
        getter = unsafeForeignFunction ["req", "name", ""] "req.params[name]"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req name)

intlReqParam ::
    forall a. (ReadForeign a) =>
    Request -> String -> ExpressM (Maybe a)
intlReqParam req name = do
    let getter = unsafeForeignFunction ["req", "name", ""] "req.param(name)"
    liftM1 (eitherToMaybe <<< parseForeign read) (getter req name)

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


