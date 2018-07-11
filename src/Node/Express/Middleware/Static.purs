module Node.Express.Middleware.Static
  ( static
  ) where

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Unit (Unit)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Request, Response)
import Prelude (($))

foreign import _static :: String -> Fn3 Request Response (Effect Unit) (Effect Unit)

-- | Handler that uses builtin 'static' middleware to serve files from specified location
static :: String -> Handler
static root = HandlerM $
  \req res nxt -> liftEffect $ runFn3 (_static root) req res nxt
