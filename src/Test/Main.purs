module Test.Main where

import Prelude

import Effect (Effect)
import Generator (generatorTest)
import Orange.Golden as Golden
import Orange.Tests (incrementTest)

main :: Effect Unit
main = do
  Golden.runSuite [
    incrementTest,
    generatorTest "basic generation" "test/golden/basic-generation.oj",
    generatorTest "function generation" "test/golden/function-generation.oj",
    generatorTest "class expression" "test/golden/class-expression.oj",
    generatorTest "block expression" "test/golden/block-expression.oj",
    generatorTest "function syntax" "test/golden/function-syntax.oj"
  ]