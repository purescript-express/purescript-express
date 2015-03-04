module Main where

import Debug.Trace
import Data.Maybe
import Data.Either
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.ST
import qualified Node.Buffer as B
import Node.Encoding
import qualified Node.FS.Async as F
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


indexHandler :: Handler
indexHandler = do
    cb <- capture sendContentsHandler
    liftEff $ F.readFile "examples/Capture.purs" cb

sendContentsHandler :: Either Error B.Buffer -> Handler
sendContentsHandler eitherContents =
    case eitherContents of
        Left err ->
            send $ show err
        Right buffer ->
            send $ B.toString UTF8 buffer

appSetup :: App
appSetup = get  "/" indexHandler

main = do
    listenHttp appSetup 8080 \_ -> trace $ "Listening on 8080"
