module Node.Express.Settings where

import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readInt, readNumber, readString, tagOf, unsafeFromForeign, unsafeToForeign)
import Node.Express.Types (Application)
import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Express.App (AppM(..))

data Etag = Etag_Disabled | Etag_Enabled | Etag_Strong | Etag_Weak | Etag_Dynamic

etagToString :: Etag -> String
etagToString =
  case _ of
    Etag_Disabled -> "false"
    Etag_Enabled -> "true"
    Etag_Strong -> "strong"
    Etag_Weak -> "weak"
    Etag_Dynamic -> "dynamic"

etagFromString :: String -> Etag
etagFromString =
  case _ of
    "false" -> Etag_Disabled
    "true" -> Etag_Enabled
    "strong" -> Etag_Strong
    "weak" -> Etag_Weak
    "dynamic" -> Etag_Dynamic
    _ -> Etag_Weak -- Default to Weak if unknown value

------------------------------------------------------------------

data TrustProxy
  = TrustProxy_Boolean Boolean
  | TrustProxy_Strings (Array String)
  | TrustProxy_Number Number
  | TrustProxy_Function (String -> Boolean)

trustProxyToForeign :: TrustProxy -> Foreign
trustProxyToForeign (TrustProxy_Boolean b) = unsafeToForeign b
trustProxyToForeign (TrustProxy_Strings arr) = unsafeToForeign arr
trustProxyToForeign (TrustProxy_Number n) = unsafeToForeign n
trustProxyToForeign (TrustProxy_Function f) = unsafeToForeign f -- Note: Functions in Foreign are tricky

trustProxyFromForeignF :: Foreign -> F TrustProxy
trustProxyFromForeignF f = do
  let tag = tagOf f
  case tag of
    "Boolean" -> do
      b <- readBoolean f
      pure $ TrustProxy_Boolean b
    "Array" -> do
      arr <- readArray f
      strings <- traverse readString arr
      pure $ TrustProxy_Strings strings
    "Number" -> do
      n <- readNumber f
      pure $ TrustProxy_Number n
    "Function" -> pure $ TrustProxy_Function $ unsafeFromForeign f
    _ -> fail $ ForeignError $ "Unexpected tag: " <> tag

trustProxyFromForeignOrThrow :: Foreign -> Effect TrustProxy
trustProxyFromForeignOrThrow f =
  case runExcept (trustProxyFromForeignF f) of
    Right trustProxy -> pure trustProxy
    Left err -> throw $ "Foreign parsing failed: " <> show err

------------------------------------------------------------------

fromForeignUsingOrThrow :: forall a. (Foreign -> F a) -> Foreign -> Effect a
fromForeignUsingOrThrow read f =
  case runExcept (read f) of
    Right trustProxy -> pure trustProxy
    Left err -> throw $ "Foreign parsing failed: " <> show err

------------------------------------------------------------------

-- Foreign imports for unsafe get and set operations
foreign import _get :: EffectFn2 Application String Foreign
foreign import _set :: EffectFn3 Application String Foreign Unit

-- String constants
-- from https://expressjs.com/en/5x/api.html#app.settings.table
case_sensitive_routing :: String
case_sensitive_routing = "case sensitive routing"

env :: String
env = "env"

etag :: String
etag = "etag"

jsonp_callback_name :: String
jsonp_callback_name = "jsonp callback name"

json_escape :: String
json_escape = "json escape"

json_replacer :: String
json_replacer = "json replacer"

json_spaces :: String
json_spaces = "json spaces"

query_parser :: String
query_parser = "query parser"

strict_routing :: String
strict_routing = "strict routing"

subdomain_offset :: String
subdomain_offset = "subdomain offset"

trust_proxy :: String
trust_proxy = "trust proxy"

views :: String
views = "views"

view_cache :: String
view_cache = "view cache"

view_engine :: String
view_engine = "view engine"

x_powered_by :: String
x_powered_by = "x-powered-by"

-- | Set case sensitive routing
set__case_sensitive_routing :: Boolean -> AppM Unit
set__case_sensitive_routing value = AppM \app -> runEffectFn3 _set app case_sensitive_routing (unsafeToForeign value)

-- | Get case sensitive routing setting
get__case_sensitive_routing :: AppM Boolean
get__case_sensitive_routing = AppM \app -> runEffectFn2 _get app case_sensitive_routing >>= fromForeignUsingOrThrow readBoolean

-- | Set environment mode
set__env :: String -> AppM Unit
set__env value = AppM \app -> runEffectFn3 _set app env (unsafeToForeign value)

-- | Get environment mode
get__env :: AppM String
get__env = AppM \app -> runEffectFn2 _get app env >>= fromForeignUsingOrThrow readString

-- | Set ETag response header
set__etag :: Etag -> AppM Unit
set__etag etagValue = AppM \app -> runEffectFn3 _set app etag (unsafeToForeign $ etagToString etagValue)

-- | Get ETag setting
get__etag :: AppM Etag
get__etag = AppM \app -> map etagFromString $ (runEffectFn2 _get app etag >>= fromForeignUsingOrThrow readString)

-- | Set JSONP callback name
set__jsonp_callback_name :: String -> AppM Unit
set__jsonp_callback_name value = AppM \app -> runEffectFn3 _set app jsonp_callback_name (unsafeToForeign value)

-- | Get JSONP callback name
get__jsonp_callback_name :: AppM String
get__jsonp_callback_name = AppM \app -> runEffectFn2 _get app jsonp_callback_name >>= fromForeignUsingOrThrow readString

-- | Set JSON escape
set__json_escape :: Boolean -> AppM Unit
set__json_escape value = AppM \app -> runEffectFn3 _set app json_escape (unsafeToForeign value)

-- | Get JSON escape setting
get__json_escape :: AppM Boolean
get__json_escape = AppM \app -> runEffectFn2 _get app json_escape >>= fromForeignUsingOrThrow readBoolean

-- | Set JSON replacer
-- TODO: either a function or an array
set__json_replacer :: Array String -> AppM Unit
set__json_replacer value = AppM \app -> runEffectFn3 _set app json_replacer (unsafeToForeign value)

-- | Get JSON replacer
get__json_replacer :: AppM (Array String)
get__json_replacer = AppM \app -> runEffectFn2 _get app json_replacer >>= fromForeignUsingOrThrow \f -> do
  a <- readArray f
  traverse readString a

-- | Set JSON spaces
set__json_spaces :: Int -> AppM Unit
set__json_spaces value = AppM \app -> runEffectFn3 _set app json_spaces (unsafeToForeign value)

-- | Get JSON spaces
get__json_spaces :: AppM Int
get__json_spaces = AppM \app -> runEffectFn2 _get app json_spaces >>= fromForeignUsingOrThrow readInt

-- | Set query parser
set__query_parser :: String -> AppM Unit
set__query_parser value = AppM \app -> runEffectFn3 _set app query_parser (unsafeToForeign value)

-- | Get query parser setting
get__query_parser :: AppM String
get__query_parser = AppM \app -> runEffectFn2 _get app query_parser >>= fromForeignUsingOrThrow readString

-- | Set strict routing
set__strict_routing :: Boolean -> AppM Unit
set__strict_routing value = AppM \app -> runEffectFn3 _set app strict_routing (unsafeToForeign value)

-- | Get strict routing setting
get__strict_routing :: AppM Boolean
get__strict_routing = AppM \app -> runEffectFn2 _get app strict_routing >>= fromForeignUsingOrThrow readBoolean

-- | Set subdomain offset
set__subdomain_offset :: Int -> AppM Unit
set__subdomain_offset value = AppM \app -> runEffectFn3 _set app subdomain_offset (unsafeToForeign value)

-- | Get subdomain offset
get__subdomain_offset :: AppM Int
get__subdomain_offset = AppM \app -> runEffectFn2 _get app subdomain_offset >>= fromForeignUsingOrThrow readInt

-- | Set trust proxy
set__trust_proxy :: TrustProxy -> AppM Unit
set__trust_proxy value = AppM \app -> runEffectFn3 _set app trust_proxy (trustProxyToForeign value)

-- | Get trust proxy setting
get__trust_proxy :: AppM TrustProxy
get__trust_proxy = AppM \app -> runEffectFn2 _get app trust_proxy >>= trustProxyFromForeignOrThrow

-- | Set views directory
set__views :: String -> AppM Unit
set__views value = AppM \app -> runEffectFn3 _set app views (unsafeToForeign value)

-- | Get views directory
get__views :: AppM String
get__views = AppM \app -> runEffectFn2 _get app views >>= fromForeignUsingOrThrow readString

-- | Set view cache
set__view_cache :: Boolean -> AppM Unit
set__view_cache value = AppM \app -> runEffectFn3 _set app view_cache (unsafeToForeign value)

-- | Get view cache setting
get__view_cache :: AppM Boolean
get__view_cache = AppM \app -> runEffectFn2 _get app view_cache >>= fromForeignUsingOrThrow readBoolean

-- | Set view engine
set__view_engine :: String -> AppM Unit
set__view_engine value = AppM \app -> runEffectFn3 _set app view_engine (unsafeToForeign value)

-- | Get view engine
get__view_engine :: AppM String
get__view_engine = AppM \app -> runEffectFn2 _get app view_engine >>= fromForeignUsingOrThrow readString

-- | Set X-Powered-By header
set__x_powered_by :: Boolean -> AppM Unit
set__x_powered_by value = AppM \app -> runEffectFn3 _set app x_powered_by (unsafeToForeign value)

-- | Get X-Powered-By header setting
get__x_powered_by :: AppM Boolean
get__x_powered_by = AppM \app -> runEffectFn2 _get app x_powered_by >>= fromForeignUsingOrThrow readBoolean
