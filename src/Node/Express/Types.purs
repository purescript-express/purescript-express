module Node.Express.Types where

import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe


foreign import data Express :: !
foreign import data Application :: *
foreign import data Event :: *
foreign import data Response :: *
foreign import data Request :: *

type ExpressM a = forall e. Eff (express :: Express | e) a

-- TODO: maybe rewrite
liftEff :: forall e a. Eff e a -> ExpressM a
liftEff = unsafeInterleaveEff

