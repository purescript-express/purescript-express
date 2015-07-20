-- These are not the FFI wrappers you are looking for --
module Node.Express.Internal.Response where

import Prelude
import Data.Foreign.EasyFFI
import Data.Foreign.Class
import Data.Maybe
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Node.Express.Internal.Utils
import Node.Express.Types


intlRespSetStatus :: forall e. Response -> Int -> ExpressM e Unit
intlRespSetStatus = unsafeForeignProcedure ["resp", "code", ""]
    "resp.status(code);"

intlRespType :: forall e. Response -> String -> ExpressM e Unit
intlRespType = unsafeForeignProcedure ["resp", "t", ""]
    "resp.type(t)"


intlRespGetHeader ::
    forall e a. (IsForeign a) =>
    Response -> String -> ExpressM e (Maybe a)
intlRespGetHeader resp field = do
    let getHeaderRaw = unsafeForeignFunction ["resp", "field", ""] "resp.get(field);"
    liftM1 (eitherToMaybe <<< read) (getHeaderRaw resp field)

intlRespSetHeader :: forall e a. Response -> String -> a -> ExpressM e Unit
intlRespSetHeader = unsafeForeignProcedure ["resp", "field", "val", ""]
    "resp.set(field, val);"


intlRespSetCookie ::
    forall e. Response -> String -> String -> CookieOptions -> ExpressM e Unit
intlRespSetCookie  = unsafeForeignProcedure
    ["resp", "name", "value", "opts", ""]
    "resp.cookie(name, value, opts);"

intlRespClearCookie ::
    forall e. Response -> String -> String -> ExpressM e Unit
intlRespClearCookie = unsafeForeignProcedure
    ["resp", "name", "path", ""]
    "resp.clearCookie(name, {path: path || '/'});"


intlRespSend :: forall e a. Response -> a -> ExpressM e Unit
intlRespSend = unsafeForeignProcedure ["resp", "data", ""]
    "resp.send(data)"

intlRespSendJson :: forall e a. Response -> a -> ExpressM e Unit
intlRespSendJson = unsafeForeignProcedure ["resp", "data", ""]
    "resp.json(data)"

intlRespSendJsonp :: forall e a. Response -> a -> ExpressM e Unit
intlRespSendJsonp = unsafeForeignProcedure ["resp", "data", ""]
    "resp.jsonp(data)"

intlRespRedirect :: forall e. Response -> Int -> String -> ExpressM e Unit
intlRespRedirect = unsafeForeignProcedure ["resp", "status", "url", ""]
    "resp.redirect(status, url)"

intlRespSetLocation :: forall e. Response -> String -> ExpressM e Unit
intlRespSetLocation = unsafeForeignProcedure ["resp", "url", ""]
    "resp.location(url)"


intlRespSetAttachment :: forall e. Response -> String -> ExpressM e Unit
intlRespSetAttachment = unsafeForeignProcedure ["resp", "filename", ""]
    "resp.attachment(filename)"

intlRespSendFile ::
    forall e opts.
    Response -> String -> { | opts } -> (Error -> ExpressM e Unit) -> ExpressM e Unit
intlRespSendFile = unsafeForeignProcedure
    ["resp", "path", "opts", "cb", ""]
    "resp.sendFile(path, opts, function(err) { return cb(err)(); })"

intlRespDownload ::
    forall e. Response -> String -> String -> (Error -> ExpressM e Unit) -> ExpressM e Unit
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
    forall e. Response -> ExpressM e Boolean
intlRespHeadersSent = unsafeForeignFunction ["resp", ""]
    "resp.headersSent"

