module Test.QueryString (testSuite) where

import Data.Either
import Data.Foldable (for_)
import Data.Maybe
import Node.Express.Internal.QueryString
import Prelude
import Test.Unit

type TestCase a b = { input :: a, expected :: b }

queryParserTestCases =
    [ {input: "a=b",        expected: Right [Param "a" "b"]}
    , {input: "a=b&b=c",    expected: Right [Param "a" "b", Param "b" "c"]}
    , {input: "a[123]=&b=", expected: Right [Param "a[123]" "", Param "b" ""]}
    , {input: "",           expected: Right []}
    , {input: "a=b%20c",    expected: Right [Param "a" "b c"]}
    , {input: "a=b+c",      expected: Right [Param "a" "b c"]}
    , {input: "a=b=c",      expected: Right [Param "a" "b=c"]}
    , {input: "a%3Db=c",    expected: Right [Param "a=b" "c"]}
    ]

queryGetOneTestCases =
    [ {input: [Param "b" "c"],                  expected: Nothing}
    , {input: [],                               expected: Nothing}
    , {input: [Param "a" "b"],                  expected: Just "b"}
    , {input: [Param "a" "b", Param "a" "c"],   expected: Just "b"}
    ]

queryGetAllTestCases =
    [ {input: [Param "b" "c"],                  expected: []}
    , {input: [],                               expected: []}
    , {input: [Param "a" "b"],                  expected: ["b"]}
    , {input: [Param "a" "b", Param "a" "c"],   expected: ["b", "c"]}
    ]

doTest :: forall a b e. (Show b, Eq b) => (a -> b) -> TestCase a b -> Assertion e
doTest fun testCase = do
    let actual = fun testCase.input
        msg = show actual ++ " should equal " ++ show testCase.expected
    assert msg (actual == testCase.expected)

testSuite = do
    test "QueryString.parse" do
        for_ queryParserTestCases $ doTest (\t -> parse t)
    test "QueryString.getOne" do
        for_ queryGetOneTestCases $ doTest (\t -> getOne t "a")
    test "QueryString.getAll" do
        for_ queryGetAllTestCases $ doTest (\t -> getAll t "a")
