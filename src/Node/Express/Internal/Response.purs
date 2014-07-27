module Node.Express.Internal.Response where

import Data.Foreign.EasyFFI
import Data.Foreign
import Data.Maybe
import Control.Monad.Eff.Class
import Node.Express.Internal.Utils
import Node.Express.Types


intlRespSetStatus :: Response -> Number -> ExpressM Unit
intlRespSetStatus = unsafeForeignProcedure ["resp", "code", ""]
    "resp.status(code);"

intlRespType :: Response -> String -> ExpressM Unit
intlRespType = unsafeForeignProcedure ["resp", "t", ""]
    "resp.type(t)"


intlRespGetHeader ::
    forall a. (ReadForeign a) =>
    Response -> String -> ExpressM (Maybe a)
intlRespGetHeader resp field = do
    let getHeaderRaw = unsafeForeignFunction ["resp", "field", ""] "resp.get(field);"
    liftM1 (eitherToMaybe <<< parseForeign read) (getHeaderRaw resp field)

intlRespSetHeader :: forall a. Response -> String -> a -> ExpressM Unit
intlRespSetHeader = unsafeForeignProcedure ["resp", "field", "val", ""]
    "resp.set(field, val);"


intlRespSetCookie ::
    forall opts.
    Response -> String -> String -> { | opts } -> ExpressM Unit
intlRespSetCookie = unsafeForeignProcedure
    ["resp", "name", "value", "opts", ""]
    "resp.cookie(name, value, opts);"

intlRespClearCookie ::
    forall opts.
    Response -> String -> { | opts } -> ExpressM Unit
intlRespClearCookie = unsafeForeignProcedure
    ["resp", "name", "opts", ""]
    "resp.clearCookie(name, opts);"


intlRespSend :: forall a. Response -> a -> ExpressM Unit
intlRespSend = unsafeForeignProcedure ["resp", "data", ""]
    "resp.send(data)"

intlRespSendJson :: forall a. Response -> a -> ExpressM Unit
intlRespSendJson = unsafeForeignProcedure ["resp", "data", ""]
    "resp.json(data)"

intlRespSendJsonp :: forall a. Response -> a -> ExpressM Unit
intlRespSendJsonp = unsafeForeignProcedure ["resp", "data", ""]
    "resp.jsonp(data)"

intlRespRedirect :: Response -> String -> ExpressM Unit
intlRespRedirect = unsafeForeignProcedure ["resp", "url", ""]
    "resp.redirect(url)"

intlRespSetLocation :: Response -> String -> ExpressM Unit
intlRespSetLocation = unsafeForeignProcedure ["resp", "url", ""]
    "resp.location(url)"


intlRespSetAttachment :: Response -> String -> ExpressM Unit
intlRespSetAttachment = unsafeForeignProcedure ["resp", "filename", ""]
    "resp.attachment(filename)"

-- TODO: sendfile, download, links
