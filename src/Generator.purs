module Generator where

import Prelude

import Compiler (Env(..), evalCompiler)
import Compiler as Compiler
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.BigInt (toString)
import Data.Either (either)
import Data.List (fold, intercalate, last, (:))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (throw)
import Node.Path (FilePath)
import Orange.Golden as Golden
import Parse (parseProgram)
import Prettier.Printer (DOC, group, line, nest, pretty, txt)
import Text.Parsing.Parser (runParser)
import Types (Expr(..), Ir(..))

generateDoc :: Ir -> Writer DOC DOC
generateDoc (IntIr int) = pure $ txt $ toString int
generateDoc (CharIr char) = pure $ txt $ show char
generateDoc (StringIr string) = pure $ txt $ show string
generateDoc (IdentIr name) = pure $ txt name
generateDoc (DotIr ir name) = do
  irDoc <- generateDoc ir
  pure $ 
    txt "(" <> 
    irDoc <>
    txt "." <>
    txt name <>
    txt ")"
generateDoc (ApplyIr ir args) = do
  irDoc <- generateDoc ir
  argsDoc <- traverse generateDoc args
  pure $ 
    irDoc <> 
    txt "(" <> 
    intercalate (txt ", ") (txt "_handle" : argsDoc) <> 
    txt ")"
generateDoc (BlockIr irs) = 
  let (Tuple irDocs docs) = runWriter $ traverse generateDoc irs
  in pure $ 
    txt "function _() {" <>
    nest 2 (
      line <> 
      docs <>
      txt "return " <>
      maybe (txt "unit") (\x -> x) (last irDocs) <>
      txt ";") <>
    line <>
    txt "}()"
generateDoc (LambdaIr args ir) = do
  irDoc <- generateDoc ir
  pure $ 
    txt "function _(" <> 
    intercalate (txt ", ") (txt "_handle" : map txt args) <> 
    txt ") {" <>
    nest 2 (
      line <>
      txt "return " <> 
      irDoc <>
      txt ";") <>
    line <>
    txt "}"
generateDoc (DoIr ir name cont) = do
  irDoc <- generateDoc ir
  contDoc <- generateDoc cont
  pure $ 
    txt "_handle(" <>
    irDoc <>
    txt ", function _(_handle, " <>
    txt name <>
    txt ") {" <>
    nest 2 (
      line <>
      txt "return " <>
      contDoc <>
      txt ";") <>
    line <>
    txt "})"
generateDoc (HandleIr ir cont) = do
  irDoc <- generateDoc ir
  contDoc <- generateDoc cont
  pure $ 
    txt "function _(_handle) {" <>
    nest 2 (
      line <>
      txt "return " <>
      irDoc <>
      txt ";") <>
    line <>
    txt "}(function _(resume, _do) {" <>
    nest 2 (
      line <>
      txt "return " <>
      contDoc <>
      txt "(_handle, _do);") <>
    line <>
    txt "})"
generateDoc (DefIr name ir) = do
  irDoc <- generateDoc ir
  tell $
    txt "const " <>
    txt name <>
    txt " = " <>
    irDoc <>
    txt ";" <> 
    line
  pure $ txt name
generateDoc (ExtendIr clazz name ir) = do
  irDoc <- generateDoc ir
  tell $
    txt "_" <>
    txt clazz <>
    txt ".prototype." <>
    txt name <>
    txt " = " <>
    irDoc <>
    txt ";" <>
    line
  pure $ txt name
generateDoc (ClassIr name args) = do
  tell $
    txt "function _" <>
    txt name <>
    txt "(" <>
    intercalate (txt ", ") (map txt args) <> 
    txt ") {" <>
    nest 2 (fold (map (\n -> line <> txt "this." <> txt n <> txt " = " <> txt n <> txt ";") args)) <>
    line <>
    txt "};" <>
    line
  tell $
    txt "function " <>
    txt name <>
    txt "(" <>
    intercalate (txt ", ") (txt "_handle" : map txt args) <> 
    txt ") { return new _" <> 
    txt name <>
    txt "(" <>
    intercalate (txt ", ") (map txt args) <>
    txt "); };" <>
    line
  pure $ txt name

generate :: Int -> Ir -> String
generate width ir = 
  let (Tuple doc docs) = runWriter $ generateDoc ir
  in pretty width $ group docs <> group doc

generatorTest :: String -> FilePath -> Golden.Test
generatorTest name path = Golden.basic name path \input -> do
  let parseResult = runParser input parseProgram
  exprs <- either (\e -> throw $ show e) pure parseResult
  let compileResult = evalCompiler (Compiler.compile $ BlockExpr exprs) (Env 0 mempty mempty)
  ir <- either (\e -> throw e) pure compileResult
  pure $ generate 80 ir
