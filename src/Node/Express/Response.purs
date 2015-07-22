module Node.Express.Response
    ( setStatus
    , getResponseHeader, setResponseHeader, headersSent, setContentType
    , setCookie, clearCookie
    , send, sendJson, sendJsonp
    , redirect, setLocation
    , sendFile, sendFileExt, download, downloadExt
    ) where

import Prelude
import Data.Foreign.EasyFFI
import Data.Foreign.Class
import Data.Maybe
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Node.Express.Handler
import Node.Express.Internal.Utils
import Node.Express.Types

-- | Set status code.
setStatus :: forall e. Int -> Handler e
setStatus val = HandlerM \_ resp _ ->
    liftEff $ intlRespSetStatus resp val

-- | Return response header value.
getResponseHeader :: forall e a. (IsForeign a) => String -> ExpressHandlerM e (Maybe a)
getResponseHeader field = HandlerM \_ resp _ -> do
    liftEff $ intlRespGetHeader resp field

-- | Set response header value.
setResponseHeader :: forall e a. String -> a -> Handler e
setResponseHeader field val = HandlerM \_ resp _ ->
    liftEff $ intlRespSetHeader resp field val

-- | Check if headers have been sent already
headersSent :: forall e. ExpressHandlerM e Boolean
headersSent = HandlerM \_ resp _ ->
    liftEff $ intlRespHeadersSent resp

-- | Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: forall e. String -> String -> CookieOptions -> Handler e
setCookie name val opts = HandlerM \_ resp _ ->
    liftEff $ intlRespSetCookie resp name val opts

-- | Clear cookie.
clearCookie :: forall e. String -> String -> Handler e
clearCookie name path = HandlerM \_ resp _ ->
    liftEff $ intlRespClearCookie resp name path

-- | Send a response. Could be object, string, buffer, etc.
send :: forall e a. a -> Handler e
send data_ = HandlerM \_ resp _ ->
    liftEff $ intlRespSend resp data_

-- | Send a JSON response. Necessary headers are set automatically.
sendJson :: forall e a. a -> Handler e
sendJson data_ = HandlerM \_ resp _ ->
    liftEff $ intlRespSendJson resp data_

-- | Send a JSON response with JSONP support.
sendJsonp :: forall e a. a -> Handler e
sendJsonp data_ = HandlerM \_ resp _ ->
    liftEff $ intlRespSendJsonp resp data_

-- | Redirect to the given URL setting status to 302.
redirect :: forall e. String -> Handler e
redirect = redirectWithStatus 302

-- | Redirect to the given URL using custom status.
redirectWithStatus :: forall e. Int -> String -> Handler e
redirectWithStatus status url = HandlerM \_ resp _ ->
    liftEff $ intlRespRedirect resp status url

-- | Set Location header.
setLocation :: forall e. String -> Handler e
setLocation url = HandlerM \_ resp _ ->
    liftEff $ intlRespSetLocation resp url

-- | Set Content-Type header.
setContentType :: forall e. String -> Handler e
setContentType t = HandlerM \_ resp _ ->
    liftEff $ intlRespType resp t

-- | Send file by its path.
sendFile :: forall e. String -> Handler e
sendFile path = sendFileExt path {root: pwd} (\_ -> return unit)
  where
    pwd = unsafeForeignFunction [] "process.cwd()"

-- | Send file by its path using specified options and error handler.
-- | See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall e o. String -> { | o } -> (Error -> ExpressM e Unit) -> Handler e
sendFileExt path opts callback = HandlerM \_ resp _ ->
    liftEff $ intlRespSendFile resp path opts callback

-- | Transfer file as an attachment (will prompt user to download).
download :: forall e. String -> Handler e
download path = downloadExt path "" (\_ -> return unit)

-- | Transfer file as an attachment using specified filename and error handler.
downloadExt :: forall e. String -> String -> (Error -> ExpressM e Unit) -> Handler e
downloadExt path filename callback = HandlerM \_ resp _ ->
    liftEff $ intlRespDownload resp path filename callback

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
