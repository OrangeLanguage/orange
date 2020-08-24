module Test.Main where

import Prelude

import Compiler (Env(..), compile, evalCompiler)
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (throw)
import Generator (generate)
import Node.Path (FilePath)
import Parse (parseProgram)
import Test.Golden as Golden
import Text.Parsing.Parser (runParser)
import Types (Expr(..))

main :: Effect Unit
main = Golden.runSuite [
  generatorTest "basic generation" "test/golden/basic-generation.oj",
  generatorTest "function generation" "test/golden/function-generation.oj",
  generatorTest "class expression" "test/golden/class-expression.oj",
  generatorTest "block expression" "test/golden/block-expression.oj",
  generatorTest "zero argument function syntax" "test/golden/zero-argument-function-syntax.oj",
  generatorTest "lazy identity" "test/golden/lazy-identity.oj",
  generatorTest "basic with" "test/golden/basic-with.oj",
  generatorTest "basic trailing block" "test/golden/basic-trailing-block.oj",
  generatorTest "lens composition" "test/golden/lens-composition.oj",
  generatorTest "basic extension" "test/golden/basic-extension.oj",
  generatorTest "multiple functions" "test/golden/multiple-function-generation.oj"
]

generatorTest :: String -> FilePath -> Golden.Test
generatorTest name path = Golden.basic name path \input -> do
  let parseResult = runParser input parseProgram
  exprs <- either (\e -> throw $ show e) pure parseResult
  let compileResult = evalCompiler (compile $ BlockExpr exprs) (Env 0 mempty mempty)
  ir <- either (\e -> throw e) pure compileResult
  pure $ generate 80 ir
