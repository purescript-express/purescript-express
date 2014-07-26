module Node.Express.Types where

import Data.Foreign
import Data.Either
import Data.Foreign.EasyFFI
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Class


foreign import data Express :: !
foreign import data Application :: *
foreign import data Event :: *
foreign import data Response :: *
foreign import data Request :: *


type ExpressM a = forall e. Eff (express :: Express | e) a

instance monadEffExpressM :: MonadEff (Eff e) where
    liftEff = unsafeInterleaveEff


data Protocol = Http | Https

instance readForeignProtocol :: ReadForeign Protocol where
    read = ForeignParser \foreign_ ->
        case parseForeign read foreign_ of
             Right "http" -> Right Http
             Right "https" -> Right Https
             _ -> Left "Unknown protocol"

