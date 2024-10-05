module Test.Main where

import Prelude

import Effect (Effect)
import Test.App as App
import Test.Handler as Handler
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  App.testSuite
  Handler.testSuite
