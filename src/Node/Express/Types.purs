module Node.Express.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Effect (Effect)
import Effect.Uncurried (EffectFn3)

foreign import data Application :: Type
foreign import data Event :: Type
foreign import data Response :: Type
foreign import data Request :: Type

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

type Host = String
type Port = Int
type Pipe = String
type Path = String

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

type Middleware = EffectFn3 Request Response (Effect Unit) Unit

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
