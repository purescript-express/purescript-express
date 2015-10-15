module Capture where

import Prelude hiding (apply)
import Data.Maybe
import Data.Either
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception
import Control.Monad.ST
import qualified Node.Buffer as B
import Node.Encoding
import qualified Node.FS.Async as F
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import Node.HTTP (Server())


indexHandler :: Handler
indexHandler = do
    cb <- capture sendContentsHandler
    liftEff $ F.readFile "src/Examples/Capture.purs" cb

sendContentsHandler :: Either Error B.Buffer -> Handler
sendContentsHandler eitherContents =
    case eitherContents of
        Left err ->
            send $ show err
        Right buffer ->
            send $ B.toString UTF8 buffer

appSetup :: App
appSetup = get  "/" indexHandler

main :: forall e. Eff (express :: Express | e) Server
main = do
    listenHttp appSetup 8080 \_ -> log $ "Listening on 8080"
