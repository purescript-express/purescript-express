module Node.Express.Internal.Utils where

import Prelude
import Data.Function.Uncurried (Fn2)
import Effect.Exception (Error)
import Effect (Effect)

foreign import nextWithError :: forall a. Fn2 (Effect Unit) Error (Effect a)
