module Node.Express.Internal.Utils where

import Prelude
import Data.Either
import Data.Function
import Data.Maybe
import Control.Monad.Eff.Exception
import Node.Express.Types


eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

foreign import nextWithError :: forall e a. Fn2 (ExpressM e Unit) Error (ExpressM e a)
