module Node.Express.Response
    ( setStatus
    , getResponseHeader, setResponseHeader, headersSent, setContentType
    , setCookie, clearCookie
    , send, sendJson, sendJsonp
    , redirect, redirectWithStatus, setLocation
    , sendFile, sendFileExt, download, downloadExt
    , render
    ) where

import Prelude
import Data.Foreign.Class (class IsForeign, read)
import Data.Function.Uncurried (Fn3, Fn4, Fn2, runFn3, runFn4, runFn2)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Internal.Utils (eitherToMaybe)
import Node.Express.Types (ExpressM, Response, CookieOptions, EXPRESS)

-- | Set status code.
setStatus :: forall e. Int -> Handler e
setStatus val = HandlerM \_ resp _ ->
    liftEff $ runFn2 _setStatus resp val

-- | Return response header value.
getResponseHeader :: forall e a. (IsForeign a) => String -> HandlerM (express :: EXPRESS | e) (Maybe a)
getResponseHeader field = HandlerM \_ resp _ -> do
    liftEff $ liftM1 (eitherToMaybe <<< runExcept <<< read) (runFn2 _getHeader resp field)

-- | Set response header value.
setResponseHeader :: forall e a. String -> a -> Handler e
setResponseHeader field val = HandlerM \_ resp _ ->
    liftEff $ runFn3 _setHeader resp field val

-- | Check if headers have been sent already
headersSent :: forall e. HandlerM (express :: EXPRESS | e) Boolean
headersSent = HandlerM \_ resp _ ->
    liftEff $ _headersSent resp

-- | Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: forall e. String -> String -> CookieOptions -> Handler e
setCookie name val opts = HandlerM \_ resp _ ->
    liftEff $ runFn4 _setCookie resp name val opts

-- | Clear cookie.
clearCookie :: forall e. String -> String -> Handler e
clearCookie name path = HandlerM \_ resp _ ->
    liftEff $ runFn3 _clearCookie resp name path

-- | Send a response. Could be object, string, buffer, etc.
send :: forall e a. a -> Handler e
send data_ = HandlerM \_ resp _ ->
    liftEff $ runFn2 _send resp data_

-- | Send a JSON response. Necessary headers are set automatically.
sendJson :: forall e a. a -> Handler e
sendJson data_ = HandlerM \_ resp _ ->
    liftEff $ runFn2 _sendJson resp data_

-- | Send a JSON response with JSONP support.
sendJsonp :: forall e a. a -> Handler e
sendJsonp data_ = HandlerM \_ resp _ ->
    liftEff $ runFn2 _sendJsonp resp data_

-- | Redirect to the given URL setting status to 302.
redirect :: forall e. String -> Handler e
redirect = redirectWithStatus 302

-- | Redirect to the given URL using custom status.
redirectWithStatus :: forall e. Int -> String -> Handler e
redirectWithStatus status url = HandlerM \_ resp _ ->
    liftEff $ runFn3 _redirectWithStatus resp status url

-- | Set Location header.
setLocation :: forall e. String -> Handler e
setLocation url = HandlerM \_ resp _ ->
    liftEff $ runFn2 _setLocation resp url

-- | Set the HTTP response Content-Disposition header field.
setAttachment :: forall e. String -> Handler e
setAttachment url = HandlerM \_ resp _ ->
    liftEff $ runFn2 _setAttachment resp url

-- | Set Content-Type header.
setContentType :: forall e. String -> Handler e
setContentType t = HandlerM \_ resp _ ->
    liftEff $ runFn2 _setContentType resp t

-- | Send file by its path.
sendFile :: forall e. String -> Handler e
sendFile path = sendFileExt path {root: _cwd unit} (\_ -> pure unit)

-- | Send file by its path using specified options and error handler.
-- | See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall e o. String -> { | o } -> (Error -> ExpressM e Unit) -> Handler e
sendFileExt path opts callback = HandlerM \_ resp _ ->
    liftEff $ runFn4 _sendFileExt resp path opts callback

-- | Transfer file as an attachment (will prompt user to download).
download :: forall e. String -> Handler e
download path = downloadExt path "" (\_ -> pure unit)

-- | Transfer file as an attachment using specified filename and error handler.
downloadExt :: forall e. String -> String -> (Error -> ExpressM e Unit) -> Handler e
downloadExt path filename callback = HandlerM \_ resp _ ->
    liftEff $ runFn4 _downloadExt resp path filename callback

-- | Render a view with a view model object. Could be object, string, buffer, etc.
render :: forall e a. String -> a -> Handler e
render view data_ = HandlerM \_ resp _ ->
    liftEff $ runFn3 _render resp view data_

foreign import _cwd :: Unit -> String

foreign import _setStatus :: forall e. Fn2 Response Int (ExpressM e Unit)

foreign import _setContentType :: forall e. Fn2 Response String (ExpressM e Unit)

foreign import _getHeader :: forall e. Fn2 Response String (ExpressM e Foreign)

foreign import _setHeader :: forall e a. Fn3 Response String a (ExpressM e Unit)

foreign import _setCookie :: forall e. Fn4 Response String String CookieOptions (ExpressM e Unit)

foreign import _clearCookie :: forall e. Fn3 Response String String (ExpressM e Unit)

foreign import _send :: forall e a. Fn2 Response a (ExpressM e Unit)

foreign import _sendJson :: forall e a. Fn2 Response a (ExpressM e Unit)

foreign import _sendJsonp :: forall e a. Fn2 Response a (ExpressM e Unit)

foreign import _redirectWithStatus :: forall e. Fn3 Response Int String (ExpressM e Unit)

foreign import _setLocation :: forall e. Fn2 Response String (ExpressM e Unit)

foreign import _setAttachment :: forall e. Fn2 Response String (ExpressM e Unit)

foreign import _sendFileExt :: forall e opts. Fn4 Response String { | opts } (Error -> ExpressM e Unit) (ExpressM e Unit)

foreign import _downloadExt :: forall e. Fn4 Response String String (Error -> ExpressM e Unit) (ExpressM e Unit)

foreign import _headersSent :: forall e. Response -> ExpressM e Boolean

foreign import _render :: forall e a. Fn3 Response String a (ExpressM e Unit)

