module Node.Express.Middleware.CookieParser
  ( cookieParser
  ) where

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Function (($))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Unit (Unit)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Request, Response)

foreign import _cookieParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

-- | Handler that parses cookies using 'cookie-parser' middleware.
cookieParser :: Handler
cookieParser = HandlerM $
  \req res nxt -> liftEffect $ runFn3 _cookieParser req res nxt
