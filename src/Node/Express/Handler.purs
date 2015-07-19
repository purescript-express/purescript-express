module Node.Express.Handler
    ( HandlerM(..)
    , Handler()
    , runHandlerM, next, nextThrow
    ) where


import Prelude
import Data.Function
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad
import Node.Express.Types
import Node.Express.Internal.Utils

-- | Monad responsible for handling single request.
data HandlerM e a = HandlerM (Request -> Response -> Eff e Unit -> Aff e a)

type Handler e = HandlerM (express :: EXPRESS | e) Unit

instance functorHandlerM :: Functor (HandlerM e) where
    map f (HandlerM h) = HandlerM \req resp nxt ->
        (h req resp nxt >>= \r -> return $ f r)

instance applyHandlerM :: Apply (HandlerM e) where
    apply (HandlerM f) (HandlerM h) = HandlerM \req resp nxt -> do
        res   <- h req resp nxt
        trans <- f req resp nxt
        return $ trans res

instance applicativeHandlerM :: Applicative (HandlerM e) where
    pure x = HandlerM \_ _ _ -> return x

instance bindHandlerM :: Bind (HandlerM e) where
    bind (HandlerM h) f = HandlerM \req resp nxt -> do
        (HandlerM g) <- liftM1 f $ h req resp nxt
        g req resp nxt

instance monadHandlerM :: Monad (HandlerM e)

instance monadEffHandlerM :: MonadEff eff (HandlerM eff) where
    liftEff act = HandlerM \_ _ _ -> liftEff act

instance monadAffHandlerM :: MonadAff eff (HandlerM eff) where
    liftAff act = HandlerM \_ _ _ -> act

runHandlerM :: forall e. Handler e -> Request -> Response -> ExpressM e Unit -> ExpressM e Unit
runHandlerM (HandlerM h) req res nxt = runAff (runFn2 nextWithError nxt) pure (h req res nxt)

-- | Call next handler/middleware in a chain.
next :: forall e. Handler e
next = HandlerM \_ _ nxt -> liftEff nxt

-- | Call next handler/middleware and pass error to it.
nextThrow :: forall e a. Error -> HandlerM (express :: EXPRESS | e) a
nextThrow err = HandlerM \_ _ nxt ->
    liftEff $ runFn2 nextWithError nxt err
