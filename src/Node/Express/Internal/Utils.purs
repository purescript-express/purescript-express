module Node.Express.Internal.Utils where

import Prelude
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Effect (Effect)


eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

foreign import nextWithError :: forall a. Fn2 (Effect Unit) Error (Effect a)
