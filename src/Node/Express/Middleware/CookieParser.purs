module Node.Express.Middleware.CookieParser
  ( cookieParser
  ) where

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Function (($))
import Data.Unit (Unit)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Request, Response)
import Effect.Uncurried (EffectFn3, runEffectFn3)

foreign import _cookieParser :: EffectFn3 Request Response (Effect Unit) Unit

-- | Handler that parses cookies using 'cookie-parser' middleware.
cookieParser :: Handler
cookieParser = HandlerM $
  \req res nxt -> liftEffect $ runEffectFn3 _cookieParser req res nxt
