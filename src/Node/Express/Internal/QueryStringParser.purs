module Node.Express.Internal.QueryStringParser
    ( Param(..)
    , parse
    ) where

import Data.Either
import Data.String
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String


data Param = Param String String

instance showParam :: Show Param where
    show (Param name val) = "(" ++ show name ++ " -> " ++ show val ++ ")"

instance eqParam :: Eq Param where
    (==) (Param n1 v1) (Param n2 v2) = (n1 == n2) && (v1 == v2)
    (/=) (Param n1 v1) (Param n2 v2) = (n1 /= n2) || (v1 /= v2)


parse :: String -> Either String [Param]
parse str = case runParser str queryString of
    Left (ParseError err) -> Left err.message
    Right result -> Right result

queryString :: Parser String [Param]
queryString = sepBy param (string "&")

param :: Parser String Param
param = do
    name <- liftM1 (joinWith "") $ many1 $ satisfy (\s -> s /= "=")
    string "="
    val  <- liftM1 (decode <<< joinWith "") $ many $ satisfy (\s -> s /= "&")
    return $ Param name val

foreign import decode
    "function decode(str) {\
    \   try {\
    \       return decodeURIComponent(str.replace(/\\+/g, ' '));\
    \   } catch(err) {\
    \       return str;\
    \   }\
    \}" :: String -> String
