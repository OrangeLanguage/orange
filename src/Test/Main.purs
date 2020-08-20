module Test.Main where

import Prelude

import Effect (Effect)
import Orange.Golden as Golden
import Orange.Tests (incrementTest)

main :: Effect Unit
main = do
  Golden.runSuite [
    incrementTest
  ]