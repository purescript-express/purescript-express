module Test.Main where

import Prelude
import Test.Unit.Main (runTest)
import Test.App as App
import Test.Handler as Handler

main = runTest do
    App.testSuite
    Handler.testSuite
