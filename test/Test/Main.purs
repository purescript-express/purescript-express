module Test.Main where

import Prelude
import Test.Unit.Main (runTest)
import Test.QueryString as QueryString
import Test.App as App
import Test.Handler as Handler

main = runTest do
    QueryString.testSuite
    App.testSuite
    Handler.testSuite
