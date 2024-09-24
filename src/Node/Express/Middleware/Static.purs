module Node.Express.Middleware.Static
  ( static
  ) where

import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Middleware)
import Prelude (($))

foreign import _static :: String -> Middleware

-- | Handler that uses builtin 'static' middleware to serve files from specified location
static :: String -> Handler
static root = HandlerM $
  \req res nxt -> liftEffect $ runEffectFn3 (_static root) req res nxt
