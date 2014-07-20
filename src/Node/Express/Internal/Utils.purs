module Node.Express.Internal.Utils where

import Data.Either
import Data.Maybe


eitherToMaybe :: forall a. Either String a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v
