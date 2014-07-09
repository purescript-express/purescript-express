module Node.Express.Internal.Response where

import Data.Foreign.EasyFFI
import Data.Foreign
import Data.Either
import Control.Monad.Eff.Class
import Node.Express.Types


intlRespStatus :: Response -> Number -> ExpressM Unit
intlRespStatus = unsafeForeignProcedure ["resp", "code", ""]
    "resp.status(code);"

intlRespGetHeader ::
    forall a. (ReadForeign a) =>
    Response
    -> String
    -> ExpressM (Either String a)
intlRespGetHeader resp field = do
    let getHeaderRaw = unsafeForeignFunction ["resp", "field", ""] "resp.get(field);"
    val <- getHeaderRaw resp field
    liftEff (Debug.Trace.print val)
    return $ parseForeign read val

intlRespSetHeader :: forall a. Response -> String -> a -> ExpressM Unit
intlRespSetHeader = unsafeForeignProcedure ["resp", "field", "val", ""]
    "resp.set(field, val);"

intlRespSetCookie ::
    forall opts.
    Response
    -> String
    -> String
    -> { | opts }
    -> ExpressM Unit
intlRespSetCookie = unsafeForeignProcedure
    ["resp", "name", "value", "opts", ""]
    "resp.cookie(name, value, opts);"

intlRespClearCookie ::
    forall opts.
    Response
    -> String
    -> { | opts }
    -> ExpressM Unit
intlRespClearCookie = unsafeForeignProcedure
    ["resp", "name", "opts", ""]
    "resp.clearCookie(name, opts);"

intlRespSend :: forall a. Response -> a -> ExpressM Unit
intlRespSend = unsafeForeignProcedure ["resp", "data", ""]
    "resp.send(data)"

intlRespJson :: forall a. Response -> a -> ExpressM Unit
intlRespJson = unsafeForeignProcedure ["resp", "data", ""]
    "resp.json(data)"

intlRespJsonp :: forall a. Response -> a -> ExpressM Unit
intlRespJsonp = unsafeForeignProcedure ["resp", "data", ""]
    "resp.jsonp(data)"

intlRespRedirect :: Response -> String -> ExpressM Unit
intlRespRedirect = unsafeForeignProcedure ["resp", "url", ""]
    "resp.redirect(url)"

intlRespLocation :: Response -> String -> ExpressM Unit
intlRespLocation = unsafeForeignProcedure ["resp", "url", ""]
    "resp.location(url)"

intlRespType :: Response -> String -> ExpressM Unit
intlRespType = unsafeForeignProcedure ["resp", "t", ""]
    "resp.type(t)"

-- TODO: foramt, attachment, sendfile, download, links
