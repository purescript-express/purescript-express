module Node.Express.Internal.Request where

import Data.Either
import Data.Foreign
import Node.Express.Types

{-
foreign import intlRespStatus
    "function intlRespStatus(resp) {\
    \   return function(code) {\
    \       return function() { resp.status(code); };\
    \   }\
    \}"
    :: Response -> Number -> ExpressM Unit
-}

