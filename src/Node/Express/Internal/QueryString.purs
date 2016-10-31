module Node.Express.Internal.QueryString
    ( Param(..)
    , parse
    , getOne
    , getAll
    ) where

import Prelude
import Data.Array (head, many, mapMaybe, some)
import Data.List (List, toUnfoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.String (satisfy, string)


data Param = Param String String

instance showParam :: Show Param where
    show (Param name val) = "(" <> show name <> " -> " <> show val <> ")"

instance eqParam :: Eq Param where
    eq (Param n1 v1) (Param n2 v2) = (n1 == n2) && (v1 == v2)

getOne :: (Array Param) -> String -> Maybe String
getOne params key = head $ getAll params key

getAll :: (Array Param) -> String -> (Array String)
getAll params key = 
    mapMaybe (\(Param name val) -> if name == key then Just val else Nothing) params

parse :: String -> Either String (Array Param)
parse str = case runParser str queryString of
    Left err -> Left $ parseErrorMessage err
    Right result -> Right $ toUnfoldable result

queryString :: Parser String (List Param)
queryString = sepBy param (string "&")

param :: Parser String Param
param = do
    name <- liftM1 (decode <<< fromCharArray) $ some $ satisfy (\s -> s /= '=')
    string "="
    val  <- liftM1 (decode <<< fromCharArray) $ many $ satisfy (\s -> s /= '&')
    pure $ Param name val

foreign import decode :: String -> String
