module Node.Express.Handler
  ( HandlerM(..)
  , Handler()
  , runHandlerM
  , runParamHandlerM
  , runErrorHandlerM
  , next
  , nextThrow
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (mkEffectFn3, mkEffectFn4, runEffectFn1)
import Node.Express.Types (HandlerFnInternal_Req_Res_Next, HandlerFnInternal_Req_Res_Next_Param, Request, Response, HandlerFnInternal_Err_Req_Res_Next)

-- | Monad responsible for handling single request.
newtype HandlerM a = HandlerM (Request -> Response -> (Maybe Error -> Effect Unit) -> Aff a)

type Handler = HandlerM Unit

instance functorHandlerM :: Functor HandlerM where
  map f (HandlerM h) = HandlerM \req resp nxt ->
    (h req resp nxt >>= \r -> pure $ f r)

instance applyHandlerM :: Apply HandlerM where
  apply (HandlerM f) (HandlerM h) = HandlerM \req resp nxt -> do
    trans <- f req resp nxt
    res <- h req resp nxt
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

runHandlerM :: Handler -> HandlerFnInternal_Req_Res_Next
runHandlerM (HandlerM h) = mkEffectFn3 \req res nxtFn ->
  runAff_
    ( either (runEffectFn1 nxtFn <<< Just >>> toNullable)
        (const $ runEffectFn1 nxtFn Nullable.null)
    )
    (h req res (runEffectFn1 nxtFn <<< toNullable))

runParamHandlerM :: (String -> Handler) -> HandlerFnInternal_Req_Res_Next_Param
runParamHandlerM mkHandlerFromParamValue = mkEffectFn4 \req res nxtFn paramValue ->
  let
    (HandlerM h) = mkHandlerFromParamValue paramValue
  in
    runAff_
      ( either (runEffectFn1 nxtFn <<< Just >>> toNullable)
          (const $ runEffectFn1 nxtFn Nullable.null)
      )
      (h req res (runEffectFn1 nxtFn <<< toNullable))

runErrorHandlerM :: (Error -> Handler) -> HandlerFnInternal_Err_Req_Res_Next
runErrorHandlerM mkHandlerFromParamValue = mkEffectFn4 \error req res nxtFn ->
  let
    (HandlerM h) = mkHandlerFromParamValue error
  in
    runAff_
      ( either (runEffectFn1 nxtFn <<< Just >>> toNullable)
          (const $ runEffectFn1 nxtFn Nullable.null)
      )
      (h req res (runEffectFn1 nxtFn <<< toNullable))

-- | Call next handler/middleware in a chain.
next :: Handler
next = HandlerM \_req _res nxt ->
  liftEffect $ nxt Nothing

-- | Call next handler/middleware and pass error to it.
nextThrow :: Error -> HandlerM Unit
nextThrow err = HandlerM \_req _res nxt ->
  liftEffect $ nxt (Just err)

instance monadThrowHandlerM :: MonadThrow Error HandlerM where
  throwError err = HandlerM \_ _ _nxt -> throwError err

instance monadErrorHandlerM ∷ MonadError Error HandlerM where
  catchError (HandlerM m) h = HandlerM $ \req res nxt ->
    catchError (m req res nxt) $ \e -> case h e of HandlerM m0 -> m0 req res nxt
