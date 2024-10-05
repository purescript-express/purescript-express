module Node.Express.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.String.Regex (Regex)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4)
import Node.Path (FilePath)

foreign import data Application :: Type
foreign import data Event :: Type
foreign import data Response :: Type -- use from purescript-node-http?
foreign import data Request :: Type

type Middleware = HandlerFnInternal_Req_Res_Next -- External Middleware

type NextFnInternal = EffectFn1 (Nullable Error) Unit
type HandlerFnInternal_Req_Res_Next = EffectFn3 Request Response NextFnInternal Unit
type HandlerFnInternal_Req_Res_Next_Param = EffectFn4 Request Response NextFnInternal String Unit
type HandlerFnInternal_Err_Req_Res_Next = EffectFn4 Error Request Response NextFnInternal Unit

data Protocol = Http | Https

instance showProtocol :: Show Protocol where
  show Http = "http"
  show Https = "https"

decodeProtocol :: String -> Maybe Protocol
decodeProtocol "http" = Just Http
decodeProtocol "https" = Just Https
decodeProtocol _ = Nothing

data Method = ALL | GET | POST | PUT | DELETE | OPTIONS | HEAD | TRACE | CustomMethod String

instance showMethod :: Show Method where
  show ALL = "all"
  show GET = "get"
  show POST = "post"
  show PUT = "put"
  show DELETE = "delete"
  show OPTIONS = "options"
  show HEAD = "head"
  show TRACE = "trace"
  show (CustomMethod method) = method

decodeMethod :: String -> Method
decodeMethod "GET" = GET
decodeMethod "POST" = POST
decodeMethod "PUT" = PUT
decodeMethod "DELETE" = DELETE
decodeMethod "OPTIONS" = OPTIONS
decodeMethod "HEAD" = HEAD
decodeMethod "TRACE" = TRACE
decodeMethod method = CustomMethod method

newtype Host = Host String
newtype Port = Port Int
newtype Status = Status Int
newtype Pipe = Pipe FilePath
type Path = String
newtype DownloadFileName = DownloadFileName String

class RoutePattern :: forall a. a -> Constraint
class RoutePattern a

instance routePath :: RoutePattern String
instance routeRegex :: RoutePattern Regex

-- class RequestParam a :
class RequestParam :: forall a. a -> Constraint
class RequestParam a

instance requestParamString :: RequestParam String
instance requestParamNumber :: RequestParam Number

-- | Cookie options
-- | - maxAge -- time in msecs
-- | - signed -- use secret to sign if true
-- | - path -- cookie path
-- | - sameSite -- "same site" cookie; can be "none", "lax", "strict"
-- | - secure -- whether the cookie is only to be sent over HTTPS
-- | - httpOnly -- whether the cookie is and not made available to JavaScript
-- | - overwrite -- whether to overwrite previously set cookies of the same name

data SameSite = None | Lax | Strict

newtype CookieOptions = CookieOptions
  { maxAge :: Int
  , signed :: Boolean
  , path :: String
  , sameSite :: SameSite
  , secure :: Boolean
  , httpOnly :: Boolean
  , overwrite :: Boolean
  }

derive instance Newtype CookieOptions _

defaultCookieOptions :: CookieOptions
defaultCookieOptions =
  CookieOptions
    { maxAge: oneYear
    , signed: false
    , path: "/"
    , sameSite: Lax
    , secure: true
    , httpOnly: true
    , overwrite: true
    }
  where
  oneYear = 365 * 24 * 60 * 60 * 1000
