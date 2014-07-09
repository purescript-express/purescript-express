module Control.Monad.Eff.Class where

import Control.Monad.Eff


class MonadEff m where
    liftEff :: forall e a. Eff e a -> m a
