module Test.Main where

import Prelude

import Compiler (Env(..), compile, evalCompiler)
import Data.Either (either)
import Data.Foldable (fold)
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
]

generatorTest :: String -> FilePath -> Golden.Test
generatorTest name path = Golden.basic name path \input -> do
  let parseResult = runParser input parseProgram
  exprs <- either (\e -> throw $ show e) pure parseResult
  compileResult <- evalCompiler (compile $ BlockExpr exprs) (Env 0 mempty)
  irs <- either (\e -> throw e) pure compileResult
  pure $ fold (map (generate 80) irs)
