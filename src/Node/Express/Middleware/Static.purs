module Node.Express.Middleware.Static
  ( static
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Express.Types (Middleware)
import Node.Path (FilePath)

foreign import _static :: EffectFn1 FilePath Middleware

-- | Handler that uses builtin 'static' middleware to serve files from specified location
static :: FilePath -> Effect Middleware
static path = runEffectFn1 _static path
