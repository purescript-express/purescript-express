module Node.Express.Response
    ( setStatus
    , getResponseHeader, setResponseHeader, headersSent, setContentType
    , setCookie, clearCookie
    , send, sendJson, sendJsonp
    , redirect, redirectWithStatus, setLocation
    , sendFile, sendFileExt, download, downloadExt
    , render
    , end
    ) where

import Prelude

import Data.Function.Uncurried (Fn3, Fn4, Fn2, runFn3, runFn4, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Response, CookieOptions)

-- | Set status code.
setStatus :: Int -> Handler
setStatus val = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _setStatus resp val

-- | Return response header value.
getResponseHeader :: String -> HandlerM (Maybe String)
getResponseHeader field = HandlerM \_ resp _ ->
    liftEffect $ runFn4 _getHeader resp field Nothing Just

-- | Set response header value.
setResponseHeader :: forall a. String -> a -> Handler
setResponseHeader field val = HandlerM \_ resp _ ->
    liftEffect $ runFn3 _setHeader resp field val

-- | Check if headers have been sent already
headersSent :: HandlerM Boolean
headersSent = HandlerM \_ resp _ ->
    liftEffect $ _headersSent resp

-- | Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: String -> String -> CookieOptions -> Handler
setCookie name val opts = HandlerM \_ resp _ ->
    liftEffect $ runFn4 _setCookie resp name val opts

-- | Clear cookie.
clearCookie :: String -> String -> Handler
clearCookie name path = HandlerM \_ resp _ ->
    liftEffect $ runFn3 _clearCookie resp name path

-- | Ends the response process.
end :: Handler
end = HandlerM \_ resp _ -> liftEffect $ _end resp

-- | Send a response. Could be object, string, buffer, etc.
send :: forall a. a -> Handler
send data_ = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _send resp data_

-- | Send a JSON response. Necessary headers are set automatically.
sendJson :: forall a. a -> Handler
sendJson data_ = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _sendJson resp data_

-- | Send a JSON response with JSONP support.
sendJsonp :: forall a. a -> Handler
sendJsonp data_ = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _sendJsonp resp data_

-- | Redirect to the given URL setting status to 302.
redirect :: String -> Handler
redirect = redirectWithStatus 302

-- | Redirect to the given URL using custom status.
redirectWithStatus :: Int -> String -> Handler
redirectWithStatus status url = HandlerM \_ resp _ ->
    liftEffect $ runFn3 _redirectWithStatus resp status url

-- | Set Location header.
setLocation :: String -> Handler
setLocation url = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _setLocation resp url

-- | Set the HTTP response Content-Disposition header field.
setAttachment :: String -> Handler
setAttachment url = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _setAttachment resp url

-- | Set Content-Type header.
setContentType :: String -> Handler
setContentType t = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _setContentType resp t

-- | Send file by its path.
sendFile :: String -> Handler
sendFile path = sendFileExt path {root: _cwd unit} (\_ -> pure unit)

-- | Send file by its path using specified options and error handler.
-- | See http://expressjs.com/4x/api.html#res.sendfile
sendFileExt :: forall o. String -> { | o } -> (Error -> Effect Unit) -> Handler
sendFileExt path opts callback = HandlerM \_ resp _ ->
    liftEffect $ runFn4 _sendFileExt resp path opts callback

-- | Transfer file as an attachment (will prompt user to download).
download :: String -> Handler
download path = downloadExt path "" (\_ -> pure unit)

-- | Transfer file as an attachment using specified filename and error handler.
downloadExt :: String -> String -> (Error -> Effect Unit) -> Handler
downloadExt path filename callback = HandlerM \_ resp _ ->
    liftEffect $ runFn4 _downloadExt resp path filename callback

-- | Render a view with a view model object. Could be object, string, buffer, etc.
render :: forall a. String -> a -> Handler
render view data_ = HandlerM \_ resp _ ->
    liftEffect $ runFn3 _render resp view data_

foreign import _cwd :: Unit -> String

foreign import _setStatus :: Fn2 Response Int (Effect Unit)

foreign import _setContentType :: Fn2 Response String (Effect Unit)

foreign import _getHeader :: Fn4 Response String (Maybe String) (String -> Maybe String) (Effect (Maybe String))

foreign import _setHeader :: forall a. Fn3 Response String a (Effect Unit)

foreign import _setCookie :: Fn4 Response String String CookieOptions (Effect Unit)

foreign import _clearCookie :: Fn3 Response String String (Effect Unit)

foreign import _end :: Response -> (Effect Unit)

foreign import _send :: forall a. Fn2 Response a (Effect Unit)

foreign import _sendJson :: forall a. Fn2 Response a (Effect Unit)

foreign import _sendJsonp :: forall a. Fn2 Response a (Effect Unit)

foreign import _redirectWithStatus :: Fn3 Response Int String (Effect Unit)

foreign import _setLocation :: Fn2 Response String (Effect Unit)

foreign import _setAttachment :: Fn2 Response String (Effect Unit)

foreign import _sendFileExt :: forall opts. Fn4 Response String { | opts } (Error -> Effect Unit) (Effect Unit)

foreign import _downloadExt :: Fn4 Response String String (Error -> Effect Unit) (Effect Unit)

foreign import _headersSent :: Response -> Effect Boolean

foreign import _render :: forall a. Fn3 Response String a (Effect Unit)
