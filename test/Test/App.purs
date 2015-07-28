module Test.App (testSuite) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Foreign.Class
import Data.Foreign.Null
import Data.Foreign.Undefined
import Data.Function
import Data.Maybe
import Node.Express.Types
import Node.Express.Internal.App
import Prelude
import Test.Unit
import Test.Unit.Console

foreign import createMockApp :: Fn0 Application

type TestExpress e a = Eff ( express :: Express, testOutput :: TestOutput | e ) a

type AssertionExpress e = Assertion ( express :: Express, testOutput :: TestOutput | e)

testGetProp ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> (TestResult -> TestExpress e Unit) -> TestExpress e Unit
testGetProp mockApp property expected callback = do
    actual <- intlAppGetProp mockApp property
    if (actual == expected)
        then callback success
        else callback $ failure (show actual ++ " should be equal " ++ show expected)

assertProperty ::
    forall a e. (Show a, Eq a, IsForeign a) =>
    Application -> String -> Maybe a -> AssertionExpress e
assertProperty mockApp property expected =
    testFn (testGetProp mockApp property expected)

testSuite = do
    let mockApp = runFn0 createMockApp
    test "App.getProp" do
        assertProperty mockApp "stringProperty" (Just "string")
        -- assertProperty mockApp "intProperty" (Just 42)
        assertProperty mockApp "floatProperty" (Just 100.1)
        assertProperty mockApp "booleanProperty" (Just true)
        assertProperty mockApp "booleanFalseProperty" (Just false)
        assertProperty mockApp "arrayProperty" (Just ["a", "b", "c"])
        assertProperty mockApp "emptyArrayProperty" (Just [] :: Maybe (Array String))
    test "App.setProp" do
        assertProperty mockApp "testProperty" (Nothing :: Maybe String)
        liftEff $ intlAppSetProp mockApp "testProperty" "OK"
        assertProperty mockApp "testProperty" (Just "OK")
