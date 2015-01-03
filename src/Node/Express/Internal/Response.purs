-- These are not the FFI wrappers you are looking for --
module Node.Express.Internal.Response where

import Data.Foreign.EasyFFI
import Data.Foreign.Class
import Data.Maybe
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Node.Express.Internal.Utils
import Node.Express.Types


intlRespSetStatus :: Response -> Number -> ExpressM Unit
intlRespSetStatus = unsafeForeignProcedure ["resp", "code", ""]
    "resp.status(code);"

intlRespType :: Response -> String -> ExpressM Unit
intlRespType = unsafeForeignProcedure ["resp", "t", ""]
    "resp.type(t)"


intlRespGetHeader ::
    forall a. (IsForeign a) =>
    Response -> String -> ExpressM (Maybe a)
intlRespGetHeader resp field = do
    let getHeaderRaw = unsafeForeignFunction ["resp", "field", ""] "resp.get(field);"
    liftM1 (eitherToMaybe <<< read) (getHeaderRaw resp field)

intlRespSetHeader :: forall a. Response -> String -> a -> ExpressM Unit
intlRespSetHeader = unsafeForeignProcedure ["resp", "field", "val", ""]
    "resp.set(field, val);"


intlRespSetCookie ::
    Response -> String -> String -> CookieOptions -> ExpressM Unit
intlRespSetCookie  = unsafeForeignProcedure
    ["resp", "name", "value", "opts", ""]
    "resp.cookie(name, value, opts);"

intlRespClearCookie ::
    Response -> String -> String -> ExpressM Unit
intlRespClearCookie = unsafeForeignProcedure
    ["resp", "name", "path", ""]
    "resp.clearCookie(name, {path: path || '/'});"


intlRespSend :: forall a. Response -> a -> ExpressM Unit
intlRespSend = unsafeForeignProcedure ["resp", "data", ""]
    "resp.send(data)"

intlRespSendJson :: forall a. Response -> a -> ExpressM Unit
intlRespSendJson = unsafeForeignProcedure ["resp", "data", ""]
    "resp.json(data)"

intlRespSendJsonp :: forall a. Response -> a -> ExpressM Unit
intlRespSendJsonp = unsafeForeignProcedure ["resp", "data", ""]
    "resp.jsonp(data)"

intlRespRedirect :: Response -> Number -> String -> ExpressM Unit
intlRespRedirect = unsafeForeignProcedure ["resp", "status", "url", ""]
    "resp.redirect(status, url)"

intlRespSetLocation :: Response -> String -> ExpressM Unit
intlRespSetLocation = unsafeForeignProcedure ["resp", "url", ""]
    "resp.location(url)"


intlRespSetAttachment :: Response -> String -> ExpressM Unit
intlRespSetAttachment = unsafeForeignProcedure ["resp", "filename", ""]
    "resp.attachment(filename)"

intlRespSendFile ::
    forall opts.
    Response -> String -> { | opts } -> (Error -> ExpressM Unit) -> ExpressM Unit
intlRespSendFile = unsafeForeignProcedure
    ["resp", "path", "opts", "cb", ""]
    "resp.sendFile(path, opts, function(err) { return cb(err)(); })"

intlRespDownload ::
    Response -> String -> String -> (Error -> ExpressM Unit) -> ExpressM Unit
intlRespDownload = unsafeForeignProcedure
    ["resp", "path", "name", "cb", ""]
    """
    if (name === "") {
        resp.download(path, function(err) { return cb(err)(); });
    } else {
        resp.download(path, name, function(err) { return cb(err)(); });
    }
    """

intlRespHeadersSent ::
    Response -> ExpressM Boolean
intlRespHeadersSent = unsafeForeignFunction ["resp", ""]
    "resp.headersSent"

