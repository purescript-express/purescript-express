module Node.Express.Middleware.Static
  ( static
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Unit (Unit)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Request, Response, ExpressM)
import Prelude (($))

foreign import _static :: forall eff. String -> Fn3 Request Response (ExpressM eff Unit) (ExpressM eff Unit)

static :: forall eff. String -> Handler eff
static root = HandlerM $
  \req res nxt -> liftEff $ runFn3 (_static root) req res nxt
