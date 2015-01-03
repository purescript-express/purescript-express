module Node.Express.Internal.QueryString
    ( Param(..)
    , parse
    , getOne
    , getAll
    ) where

import Data.Either
import Data.Maybe
import Data.String
import Control.Alternative
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String


data Param = Param String String

instance showParam :: Show Param where
    show (Param name val) = "(" ++ show name ++ " -> " ++ show val ++ ")"

instance eqParam :: Eq Param where
    (==) (Param n1 v1) (Param n2 v2) = (n1 == n2) && (v1 == v2)
    (/=) (Param n1 v1) (Param n2 v2) = (n1 /= n2) || (v1 /= v2)

getOne :: [Param] -> String -> Maybe String
getOne [] _ = Nothing
getOne ((Param name val):_) key | name == key = Just val
getOne (_:ps) key = getOne ps key

getAll :: [Param] -> String -> [String]
getAll params key = go params [] where
    go [] acc = acc
    go ((Param name val):ps) acc | name == key = go ps (acc ++ [val])
    go (_:ps) acc = go ps acc


parse :: String -> Either String [Param]
parse str = case runParser str queryString of
    Left (ParseError err) -> Left err.message
    Right result -> Right result

queryString :: Parser String [Param]
queryString = sepBy param (string "&")

param :: Parser String Param
param = do
    name <- liftM1 (decode <<< joinWith "") $ some $ satisfy (\s -> s /= "=")
    string "="
    val  <- liftM1 (decode <<< joinWith "") $ many $ satisfy (\s -> s /= "&")
    return $ Param name val

foreign import decode
    """
    function decode(str) {
        try {
            return decodeURIComponent(str.replace(/\+/g, ' '));
        } catch(err) {
            return str;
        }
    }
    """ :: String -> String
