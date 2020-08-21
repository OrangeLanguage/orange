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
  generatorTest "function syntax" "test/golden/function-syntax.oj",
  generatorTest "zero argument function syntax" "test/golden/zero-argument-function-syntax.oj"
]

generatorTest :: String -> FilePath -> Golden.Test
generatorTest name path = Golden.basic name path \input -> do
  let parseResult = runParser input parseProgram
  exprs <- either (\e -> throw $ show e) pure parseResult
  let compileResult = evalCompiler (compile $ BlockExpr exprs) (Env 0 mempty mempty)
  ir <- either (\e -> throw e) pure compileResult
  pure $ generate 80 ir
