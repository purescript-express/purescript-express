module Node.Express.Middleware.CookieParser
  ( cookieParser
  ) where

import Effect.Class (liftEffect)
import Data.Function (($))
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Middleware)
import Effect.Uncurried (runEffectFn3)

foreign import _cookieParser :: Middleware

-- | Handler that parses cookies using 'cookie-parser' middleware.
cookieParser :: Handler
cookieParser = HandlerM $
  \req res nxt -> liftEffect $ runEffectFn3 _cookieParser req res nxt
