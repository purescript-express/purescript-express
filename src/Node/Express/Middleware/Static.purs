module Node.Express.Middleware.Static
  ( static
  ) where

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Unit (Unit)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Request, Response)
import Prelude (($))
import Effect.Uncurried (EffectFn3, runEffectFn3)

foreign import _static :: String -> EffectFn3 Request Response (Effect Unit) Unit

-- | Handler that uses builtin 'static' middleware to serve files from specified location
static :: String -> Handler
static root = HandlerM $
  \req res nxt -> liftEffect $ runEffectFn3 (_static root) req res nxt
