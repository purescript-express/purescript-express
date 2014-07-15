module Node.Express.Internal.Request where

import Data.Either
import Data.Foreign
import Data.Foreign.EasyFFI
import Node.Express.Types


class RequestParam a
instance requestParamString :: RequestParam String
instance requestParamNumber :: RequestParam Number

intlReqParams ::
    forall a. (RequestParam a) =>
    Request
    -> a
    -> ExpressM (Either String String)
intlReqParams req param = do
    let getter :: forall a. Request -> a -> ExpressM Foreign
        getter = unsafeForeignFunction ["req", "param", ""] "req.params[param]"
    val <- getter req param
    return $ parseForeign read val

intlReqGetHeader ::
    forall a. (ReadForeign a) =>
    Request
    -> String
    -> ExpressM (Either String a)
intlReqGetHeader req field = do
    let getHeaderRaw = unsafeForeignFunction ["req", "field", ""] "req.get(field);"
    val <- getHeaderRaw req field
    return $ parseForeign read val
