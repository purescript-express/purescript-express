module Test.Main where

import Prelude
import Test.Unit

main = runTest do
    Test.QueryString.testSuite
