module Node.Express.Response
  ( Dotfiles(..)
  , setStatus
  , getResponseHeader
  , setResponseHeader
  , headersSent
  , setContentType
  , setCookie
  , setAttachment
  , clearCookie
  , send
  , sendJson
  , sendJsonp
  , redirect
  , redirectWithStatus
  , setLocation
  , sendFile
  , sendFileExt
  , defaultSendFileOptions
  , download
  , downloadExt
  , defaultDownloadOptions
  , render
  , end
  ) where

import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (CookieOptions(..), DownloadFileName, Response, SameSite(..), Status(..))
import Node.Path (FilePath)
import Record (modify)
import Type.Prelude (Proxy(..))

-- | Set status code.
setStatus :: Int -> Handler
setStatus val = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _setStatus resp val

-- | Return response header value.
getResponseHeader :: String -> HandlerM (Maybe String)
getResponseHeader field = HandlerM \_ resp _ ->
  liftEffect $ map Nullable.toMaybe $ runEffectFn2 _getHeader resp field

-- | Set response header value.
setResponseHeader :: forall a. String -> a -> Handler
setResponseHeader field val = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn3 _setHeader resp field val

-- | Check if headers have been sent already
headersSent :: HandlerM Boolean
headersSent = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn1 _headersSent resp

-- | Set cookie by its name using specified options (maxAge, path, etc).
setCookie :: String -> String -> CookieOptions -> Handler
setCookie name val (CookieOptions opts) = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn4 _setCookie resp name val $ modify (Proxy :: Proxy "sameSite") fromSameSite opts
  where
  fromSameSite = case _ of
    None -> "none"
    Lax -> "lax"
    Strict -> "strict"

-- | Clear cookie.
clearCookie :: String -> String -> Handler
clearCookie name path = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn3 _clearCookie resp name path

-- | Ends the response process.
end :: Handler
end = HandlerM \_ resp _ -> liftEffect $ runEffectFn1 _end resp

-- | Send a response. Could be object, string, buffer, etc.
send :: forall a. a -> Handler
send data_ = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _send resp data_

-- | Send a JSON response. Necessary headers are set automatically.
sendJson :: forall a. a -> Handler
sendJson data_ = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _sendJson resp data_

-- | Send a JSON response with JSONP support.
sendJsonp :: forall a. a -> Handler
sendJsonp data_ = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _sendJsonp resp data_

-- | Redirect to the given URL setting status to 302.
redirect :: String -> Handler
redirect = redirectWithStatus (Status 302)

-- | Redirect to the given URL using custom status.
redirectWithStatus :: Status -> String -> Handler
redirectWithStatus status url = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn3 _redirectWithStatus resp status url

-- | Set Location header.
setLocation :: String -> Handler
setLocation url = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _setLocation resp url

-- | Set the HTTP response Content-Disposition header field.
setAttachment :: String -> Handler
setAttachment url = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _setAttachment resp url

-- | Set Content-Type header.
setContentType :: String -> Handler
setContentType t = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn2 _setContentType resp t

-- | Send file by its path.
sendFile :: FilePath -> (Error -> Effect Unit) -> Handler
sendFile path handleError = sendFileExt path defaultSendFileOptions handleError

-- | Send file by its path using specified options and error handler.
-- | See http://expressjs.com/4x/api.html#res.sendfile
-- | TODO: Next handler next should handle error?
sendFileExt :: FilePath -> SendFileOptions -> (Error -> Effect Unit) -> Handler
sendFileExt path opts handleError = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn4 _sendFileExt resp path (sendFileOptionsToInternal opts)
    (mkEffectFn1 \nullableError -> maybe (pure unit) handleError $ Nullable.toMaybe nullableError)

-- | Transfer file as an attachment (will prompt user to download).
download :: FilePath -> DownloadOptions -> (Error -> Effect Unit) -> Handler
download path opts handleError = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn4 _downloadExt resp path (downloadOptionsToInternal opts)
    (mkEffectFn1 \nullableError -> maybe (pure unit) handleError $ Nullable.toMaybe nullableError)

-- | Transfer file as an attachment using specified filename and error handler.
downloadExt :: FilePath -> DownloadFileName -> DownloadOptions -> (Error -> Effect Unit) -> Handler
downloadExt path filename opts handleError = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn5 _downloadExtWithName resp path filename (downloadOptionsToInternal opts)
    (mkEffectFn1 \nullableError -> maybe (pure unit) handleError $ Nullable.toMaybe nullableError)

-- | Render a view with a view model object. Could be object, string, buffer, etc.
render :: forall a. String -> a -> Handler
render view data_ = HandlerM \_ resp _ ->
  liftEffect $ runEffectFn3 _render resp view data_

---------------------------------------------------------------------------

data Dotfiles = Dotfiles_Allow | Dotfiles_Deny | Dotfiles_Ignore

-- | Helper function to convert Dotfiles to String.
dotfilesToString :: Dotfiles -> String
dotfilesToString Dotfiles_Allow = "allow"
dotfilesToString Dotfiles_Deny = "deny"
dotfilesToString Dotfiles_Ignore = "ignore"

---------------------------------------------------------------------------

type SendFileOptions =
  { maxAge :: Int -- ^ Sets the max-age property of the Cache-Control header in milliseconds or a string in ms format
  , root :: Maybe FilePath -- ^ Root directory for relative filenames
  , lastModified :: Boolean -- ^ Sets the Last-Modified header to the last modified date of the file on the OS. Set false to disable it.
  , headers :: Object String -- ^ Object containing HTTP headers to serve with the file
  , dotfiles :: Dotfiles -- ^ Option for serving dotfiles. Possible values are "allow", "deny", "ignore"
  , acceptRanges :: Boolean -- ^ Enable or disable accepting ranged requests
  , cacheControl :: Boolean -- ^ Enable or disable setting Cache-Control response header
  , immutable :: Boolean -- ^ Enable or disable the immutable directive in the Cache-Control response header
  }

-- | Internal representation of SendFileOptions.
type SendFileOptionsInternal =
  { maxAge :: Int
  , root :: Nullable FilePath
  , lastModified :: Boolean
  , headers :: Object String
  , dotfiles :: String
  , acceptRanges :: Boolean
  , cacheControl :: Boolean
  , immutable :: Boolean
  }

-- | Default options for sending files.
defaultSendFileOptions :: SendFileOptions
defaultSendFileOptions =
  { maxAge: 0
  , root: Nothing
  , lastModified: true
  , headers: Object.empty
  , dotfiles: Dotfiles_Ignore
  , acceptRanges: true
  , cacheControl: true
  , immutable: false
  }

-- | Function to convert SendFileOptions to SendFileOptionsInternal.
sendFileOptionsToInternal :: SendFileOptions -> SendFileOptionsInternal
sendFileOptionsToInternal options =
  { maxAge: options.maxAge
  , root: Nullable.toNullable options.root
  , lastModified: options.lastModified
  , headers: options.headers
  , dotfiles: dotfilesToString options.dotfiles
  , acceptRanges: options.acceptRanges
  , cacheControl: options.cacheControl
  , immutable: options.immutable
  }

---------------------------------------------------------------------------

type DownloadOptions =
  { maxAge :: Int -- ^ Sets the max-age property of the Cache-Control header in milliseconds or a string in ms format
  , lastModified :: Boolean -- ^ Sets the Last-Modified header to the last modified date of the file on the OS. Set false to disable it.
  , headers :: Object String -- ^ Object containing HTTP headers to serve with the file
  , dotfiles :: Dotfiles -- ^ Option for serving dotfiles. Possible values are "allow", "deny", "ignore"
  , acceptRanges :: Boolean -- ^ Enable or disable accepting ranged requests
  , cacheControl :: Boolean -- ^ Enable or disable setting Cache-Control response header
  , immutable :: Boolean -- ^ Enable or disable the immutable directive in the Cache-Control response header
  }

type DownloadOptionsInternal =
  { maxAge :: Int
  , lastModified :: Boolean
  , headers :: Object String
  , dotfiles :: String
  , acceptRanges :: Boolean
  , cacheControl :: Boolean
  , immutable :: Boolean
  }

-- | Default options for sending files.
defaultDownloadOptions :: DownloadOptions
defaultDownloadOptions =
  { maxAge: 0
  , lastModified: true
  , headers: Object.empty
  , dotfiles: Dotfiles_Ignore
  , acceptRanges: true
  , cacheControl: true
  , immutable: false
  }

downloadOptionsToInternal :: DownloadOptions -> DownloadOptionsInternal
downloadOptionsToInternal options =
  { maxAge: options.maxAge
  , lastModified: options.lastModified
  , headers: options.headers
  , dotfiles: dotfilesToString options.dotfiles
  , acceptRanges: options.acceptRanges
  , cacheControl: options.cacheControl
  , immutable: options.immutable
  }

---------------------------------------------------------------------------
type SetCookieOptions =
  { maxAge :: Int
  , signed :: Boolean
  , path :: String
  , sameSite :: String
  , secure :: Boolean
  , httpOnly :: Boolean
  , overwrite :: Boolean
  }

foreign import _cwd :: Effect String
foreign import _setStatus :: EffectFn2 Response Int Unit
foreign import _setContentType :: EffectFn2 Response String Unit
foreign import _getHeader :: EffectFn2 Response String (Nullable String)
foreign import _setHeader :: forall a. EffectFn3 Response String a Unit
foreign import _setCookie :: EffectFn4 Response String String SetCookieOptions Unit
foreign import _clearCookie :: EffectFn3 Response String String Unit
foreign import _end :: EffectFn1 Response Unit
foreign import _send :: forall a. EffectFn2 Response a Unit
foreign import _sendJson :: forall a. EffectFn2 Response a Unit
foreign import _sendJsonp :: forall a. EffectFn2 Response a Unit
foreign import _redirectWithStatus :: EffectFn3 Response Status String Unit
foreign import _setLocation :: EffectFn2 Response String Unit
foreign import _setAttachment :: EffectFn2 Response String Unit
foreign import _sendFileExt :: EffectFn4 Response FilePath SendFileOptionsInternal (EffectFn1 (Nullable Error) Unit) Unit
foreign import _downloadExt :: EffectFn4 Response FilePath DownloadOptionsInternal (EffectFn1 (Nullable Error) Unit) Unit
foreign import _downloadExtWithName :: EffectFn5 Response FilePath DownloadFileName DownloadOptionsInternal (EffectFn1 (Nullable Error) Unit) Unit
foreign import _headersSent :: EffectFn1 Response Boolean
foreign import _render :: forall a. EffectFn3 Response String a Unit
