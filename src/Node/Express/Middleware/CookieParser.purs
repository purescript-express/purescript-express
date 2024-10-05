module Node.Express.Middleware.CookieParser
  ( cookieParser
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Express.Types (Middleware)

foreign import _cookieParser :: EffectFn1 (Nullable String) Middleware

-- | Handler that parses cookies using 'cookie-parser' middleware.
cookieParser :: Maybe String -> Effect Middleware
cookieParser maybeSecret = runEffectFn1 _cookieParser (Nullable.toNullable maybeSecret)
