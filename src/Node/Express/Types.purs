module Node.Express.Types where

import Data.Foreign
import Data.Either
import Data.Foreign.EasyFFI
import Data.String.Regex
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Class


foreign import data Express :: !

--| General monad, indicates that we're dealing with
--  express.js related functions
--  Applications should use HandlerM and AppM primarily
--  and ExpressM in rare cases
type ExpressM a = forall e. Eff (express :: Express | e) a

instance monadEffExpressM :: MonadEff (Eff e) where
    liftEff = unsafeInterleaveEff


foreign import data Application :: *
foreign import data Event :: *
foreign import data Error :: *
foreign import data Response :: *
foreign import data Request :: *

error :: String -> Error
error = unsafeForeignFunction ["msg"] "new Error(msg);"


data Protocol = Http | Https

instance readForeignProtocol :: ReadForeign Protocol where
    read = ForeignParser \foreign_ ->
        case parseForeign read foreign_ of
             Right "http"  -> Right Http
             Right "https" -> Right Https
             _ -> Left "Unknown protocol"


data Method = ALL | GET | POST | PUT | DELETE

instance showMethod :: Show Method where
    show ALL    = "all"
    show GET    = "get"
    show POST   = "post"
    show PUT    = "put"
    show DELETE = "delete"


type Port = Number
type Path = String

class RoutePattern a
instance routePath  :: RoutePattern String
instance routeRegex :: RoutePattern Regex

class RequestParam a
instance requestParamString :: RequestParam String
instance requestParamNumber :: RequestParam Number
