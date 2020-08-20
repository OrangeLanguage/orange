module Generator where

import Prelude

import Compiler (Env(..), evalCompiler)
import Compiler as Compiler
import Data.BigInt (toString)
import Data.Either (either)
import Data.List (fold, intercalate, (:))
import Effect.Exception (throw)
import Node.Path (FilePath)
import Orange.Golden as Golden
import Parse (parseProgram)
import Prettier.Printer (DOC, group, line, nest, pretty, txt)
import Text.Parsing.Parser (runParser)
import Types (Expr(..), Ir(..))

generateDoc :: Ir -> DOC
generateDoc (IntIr int) = txt $ toString int
generateDoc (CharIr char) = txt $ show char
generateDoc (StringIr string) = txt $ show string
generateDoc (IdentIr name) = txt name
generateDoc (DotIr ir name) =
  txt "(" <>
  generateDoc ir <>
  txt "." <>
  txt name <>
  txt ")"
generateDoc (ApplyIr ir args) =
  generateDoc ir <> 
  txt "(" <> 
  intercalate (txt ", ") (txt "_handle" : map generateDoc args) <> 
  txt ")"
generateDoc (BlockIr irs) =
  txt "function _() {" <>
  fold (map (\ir -> generateDoc ir <> txt ";" <> line) irs) <>
  txt "}()"
generateDoc (LambdaIr args ir) = 
  txt "function _(" <> 
  intercalate (txt ", ") (txt "_handle" : map txt args) <> 
  txt ") {" <>
  nest 2 (
    line <>
    txt "return " <> 
    generateDoc ir <>
    txt ";") <>
  line <>
  txt "}"
generateDoc (DoIr ir name cont) =
  txt "_handle(" <>
  generateDoc ir <>
  txt ", function _(_handle, " <>
  txt name <>
  txt ") {" <>
  nest 2 (
    line <>
    txt "return " <>
    generateDoc cont <>
    txt ";") <>
  line <>
  txt "})"
generateDoc (HandleIr ir cont) =
  txt "function _(_handle) {" <>
  nest 2 (
    line <>
    txt "return " <>
    generateDoc ir <>
    txt ";") <>
  line <>
  txt "}(function _(resume, _do) {" <>
  nest 2 (
    line <>
    txt "return " <>
    generateDoc cont <>
    txt "(_handle, _do);") <>
  line <>
  txt "})"
generateDoc (DefIr name ir) =
  txt "const " <>
  txt name <>
  txt " = " <>
  generateDoc ir <>
  txt ";"
generateDoc (ExtendIr clazz name ir) =
  txt "_" <>
  txt clazz <>
  txt ".prototype." <>
  txt name <>
  txt " = " <>
  generateDoc ir <>
  txt ";"
generateDoc (ClassIr name args) =
  txt "function _" <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map txt args) <> 
  txt ") {" <>
  nest 2 (fold (map (\n -> line <> txt "this." <> txt n <> txt " = " <> txt n <> txt ";") args)) <>
  line <>
  txt "};" <>
  line <>
  txt "function " <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (txt "_handle" : map txt args) <> 
  txt ") { return new _" <> 
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map txt args) <>
  txt "); };"

generate :: Int -> Ir -> String
generate width ir = pretty width $ group $ generateDoc ir

generatorTest :: String -> FilePath -> Golden.Test
generatorTest name path = Golden.basic name path \input -> do
  let parseResult = runParser input parseProgram
  exprs <- either (\e -> throw $ show e) pure parseResult
  let compileResult = evalCompiler (Compiler.compile $ BlockExpr exprs) (Env 0 mempty mempty)
  ir <- either (\e -> throw e) pure compileResult
  pure $ generate 0 ir

basicGeneratorTest :: Golden.Test
basicGeneratorTest = generatorTest "basic generation" "test/golden/basic-generation.oj"

functionGeneratorTest :: Golden.Test
functionGeneratorTest = generatorTest "function generation" "test/golden/function-generation.oj"