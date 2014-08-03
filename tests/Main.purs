module Main where

import Debug.Trace
import Data.Either
import Data.Foldable (for_)
import Control.Monad.Eff
import Node.Express.Internal.QueryStringParser


type Test a b = { input :: a, output :: b }


doTest :: forall a b e. (Show b, Eq b)
       => (Test a b -> b) -> Test a b -> Eff ( trace :: Trace | e) Unit
doTest testFn test =
    let result = testFn test
        printPassed = trace "PASSED"
        printFailed =
            trace ("FAILED\n\tExpected: \""
                  ++ show test.output
                  ++ "\"\n\tGot: \""
                  ++ show result
                  ++ "\"")
    in if result /= test.output then printFailed else printPassed

queryParserTests =
    [ {input: "a=b",        output: Right [Param "a" "b"]}
    , {input: "a=b&b=c",    output: Right [Param "a" "b", Param "b" "c"]}
    , {input: "a[123]=&b=", output: Right [Param "a[123]" "", Param "b" ""]}
    , {input: "",           output: Right []}
    , {input: "a=b%20c",    output: Right [Param "a" "b c"]}
    , {input: "a=b+c",      output: Right [Param "a" "b c"]}
    ]

main = do
    for_ queryParserTests (doTest (\t -> parse t.input))
