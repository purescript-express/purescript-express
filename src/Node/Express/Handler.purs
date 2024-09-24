module Node.Express.Handler
    ( HandlerM(..)
    , Handler()
    , runHandlerM, next, nextThrow
    ) where


import Prelude

import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Data.Either (either)
import Data.Function.Uncurried (Fn2, runFn2)
import Node.Express.Types (Response, Request)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)

-- | Monad responsible for handling single request.
newtype HandlerM a = HandlerM (Request -> Response -> Effect Unit -> Aff a)

type Handler = HandlerM Unit

instance functorHandlerM :: Functor HandlerM where
    map f (HandlerM h) = HandlerM \req resp nxt ->
        (h req resp nxt >>= \r -> pure $ f r)

instance applyHandlerM :: Apply HandlerM where
    apply (HandlerM f) (HandlerM h) = HandlerM \req resp nxt -> do
        trans <- f req resp nxt
        res   <- h req resp nxt
        pure $ trans res

instance applicativeHandlerM :: Applicative HandlerM where
    pure x = HandlerM \_ _ _ -> pure x

instance bindHandlerM :: Bind HandlerM where
    bind (HandlerM h) f = HandlerM \req resp nxt -> do
        (HandlerM g) <- liftM1 f $ h req resp nxt
        g req resp nxt

instance monadHandlerM :: Monad HandlerM

instance monadEffHandlerM :: MonadEffect HandlerM where
    liftEffect act = HandlerM \_ _ _ -> liftEffect act

instance monadAffHandlerM :: MonadAff HandlerM where
    liftAff act = HandlerM \_ _ _ -> act

runHandlerM :: Handler -> Request -> Response -> Effect Unit -> Effect Unit
runHandlerM (HandlerM h) req res nxt = void $ runAff_ (either (runFn2 _nextWithError nxt) pure) (h req res nxt)

-- | Call next handler/middleware in a chain.
next :: Handler
next = HandlerM \_ _ nxt -> liftEffect nxt

-- | Call next handler/middleware and pass error to it.
nextThrow :: forall a. Error -> HandlerM a
nextThrow err = HandlerM \_ _ nxt ->
    liftEffect $ runFn2 _nextWithError nxt err

instance monadThrowHandlerM :: MonadThrow Error HandlerM where
    throwError err = HandlerM \_ _ nxt -> throwError err

instance monadErrorHandlerM ∷ MonadError Error HandlerM where
    catchError (HandlerM m) h = HandlerM $ \req res nxt ->
        catchError (m req res nxt) $ \e -> case h e of HandlerM m0 -> m0 req res nxt

foreign import _nextWithError :: forall a. Fn2 (Effect Unit) Error (Effect a)
