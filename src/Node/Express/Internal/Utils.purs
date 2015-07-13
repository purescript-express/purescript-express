module Node.Express.Internal.Utils where

import Prelude
import Data.Either
import Data.Maybe
import Data.Foreign.EasyFFI
import Control.Monad.Eff.Exception
import Node.Express.Types


eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

intlNextWithError :: forall a. ExpressM Unit -> Error -> ExpressM a
intlNextWithError = unsafeForeignProcedure ["nxt", "err", ""] "nxt(err);"
