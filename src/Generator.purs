module Generator where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.BigInt (toString)
import Data.List (fold, init, intercalate, last, (:))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Prettier.Printer (DOC, group, line, nest, nil, pretty, txt)
import Types (Eval(..), Ir(..))

evalDoc :: Eval -> String -> DOC
evalDoc EagerEval name = txt name <> txt " = " <> txt name <> txt "();" <> line
evalDoc LazyEval name = nil

classArgDoc :: String -> DOC
classArgDoc name = 
  line <>
  txt "this." <> 
  txt name <>
  txt " = { ..." <>
  txt name <>
  txt ", set: function _(_handle, " <>
  txt name <>
  txt ") { return { ...this, " <>
  txt name <> 
  txt " }; } };"

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
  argsDoc <- traverse (\arg -> generateDoc arg <#> (<>) (txt "() => ")) args
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
      maybe nil (fold <<< map (\x -> x <> txt ";" <> line)) (init irDocs) <>
      txt "return " <>
      maybe (txt "unit") (\x -> x) (last irDocs) <>
      txt ";") <>
    line <>
    txt "}()"
generateDoc (LambdaIr args ir) = do
  irDoc <- generateDoc ir
  pure $ 
    txt "function _(" <> 
    intercalate (txt ", ") (txt "_handle" : map (snd >>> txt) args) <> 
    txt ") {" <>
    nest 2 (
      line <>
      fold (map (uncurry evalDoc) args) <>
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
    nest 2 (fold (map classArgDoc args)) <>
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
generateDoc (MixinIr name) = pure $ 
  txt "function _(_handle, _object, _mixin) {" <>
  nest 2 (
    line <>
    txt "return { ..._object, " <>
    txt name <>
    txt ": _mixin };") <>
  line <>
  txt "}" 

generate :: Int -> Ir -> String
generate width ir = 
  let (Tuple doc docs) = runWriter $ generateDoc ir
  in pretty width $ group docs <> group doc
