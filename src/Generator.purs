module Generator where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (elem)
import Data.BigInt (toString)
import Data.List (fold, init, intercalate, last, (:))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, group, line, nest, nil, pretty, txt)
import Types (Arg(..), Eval(..), Ir(..))

escape :: String -> String
escape word = if elem word [
  "abstract"  ,"arguments"  ,"await"        ,"boolean"    ,
  "break"     ,"byte"       ,"case"         ,"catch"      ,
  "char"      ,"class"      ,"const"        ,"continue"   ,
  "debugger"  ,"default"    ,"delete"       ,"do"         ,
  "double"    ,"else"       ,"enum"         ,"eval"       ,
  "export"    ,"extends"    ,"false"        ,"final"      ,
  "finally"   ,"float"      ,"for"          ,"function"   ,
  "goto"      ,"if"         ,"implements"   ,"import"     ,
  "in"        ,"instanceof" ,"int"          ,"interface"  ,
  "let"       ,"long"       ,"native"       ,"new"        ,
  "null"      ,"package"    ,"private"      ,"protected"  ,
  "public"    ,"return"     ,"short"        ,"static"     ,
  "super"     ,"switch"     ,"synchronized" ,
  "throw"     ,"throws"     ,"transient"    ,"true"       ,
  "try"       ,"typeof"     ,"var"          ,"void"       ,
  "volatile"  ,"while"      ,"with"         ,"yield"      ]
  then "_" <> word
  else word

argDoc :: Arg -> DOC
argDoc (Arg EagerEval name) = txt (escape name) <> txt " = " <> txt (escape name) <> txt "();" <> line
argDoc (Arg LazyEval name) = nil

classArgDoc :: String -> DOC
classArgDoc name = 
  line <>
  txt "this." <> 
  txt (escape name) <>
  txt " = " <>
  txt (escape name) <> 
  txt ";"

generateDoc :: Ir -> Writer DOC DOC
generateDoc (BoolIr bool) = pure $ txt $ if bool then "_true" else "_false"
generateDoc (IntIr int) = pure $ txt $ "new _Int(" <> toString int <> ")"
generateDoc (CharIr char) = pure $ txt $ "new _Char(" <> show char <> ")"
generateDoc (StringIr string) = pure $ txt $ "new _String(" <> show string <> ")"
generateDoc (IdentIr name) = pure $ txt (escape name)
generateDoc (DotIr ir name) = do
  irDoc <- generateDoc ir
  pure $ 
    txt "(" <> 
    irDoc <>
    txt "." <>
    txt (escape name) <>
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
    txt "(() => {" <>
    nest 2 (
      line <> 
      docs <>
      maybe nil (fold <<< map (\x -> x <> txt ";" <> line)) (init irDocs) <>
      txt "return " <>
      maybe (txt "_unit") (\x -> x) (last irDocs) <>
      txt ";") <>
    line <>
    txt "})()"
generateDoc (LambdaIr args ir) = do
  irDoc <- generateDoc ir
  pure $ 
    txt "(function (" <> 
    intercalate (txt ", ") (txt "_handle" : map (\(Arg eval name) -> txt (escape name)) args) <> 
    txt ") {" <>
    nest 2 (
      line <>
      fold (map argDoc args) <>
      txt "return " <> 
      irDoc <>
      txt ";") <>
    line <>
    txt "})"
generateDoc (DoIr ir name cont) = do
  irDoc <- generateDoc ir
  contDoc <- generateDoc cont
  pure $ 
    txt "_handle(() => " <>
    irDoc <>
    txt ", (_handle, " <>
    txt (escape name) <>
    txt ") => {" <>
    nest 2 (
      line <>
      txt (escape name) <>
      txt " = " <>
      txt (escape name) <>
      txt "();" <>
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
    txt "((_handle) => " <>
    irDoc <>
    txt ")((_do, resume) => " <>
    contDoc <>
    txt "(_handle, _do)" <>
    txt ")"
generateDoc (DefIr name ir) = do
  irDoc <- generateDoc ir
  tell $
    txt "const " <>
    txt (escape name) <>
    txt " = " <>
    irDoc <>
    txt ";" <> 
    line
  pure $ txt (escape name)
generateDoc (ExtendIr clazz name ir) = do
  irDoc <- generateDoc ir
  tell $
    txt "_" <>
    txt clazz <>
    txt ".prototype." <>
    txt (escape name) <>
    txt " = " <>
    irDoc <>
    txt ";" <>
    line
  pure $ 
    txt "_" <>
    txt clazz <>
    txt ".prototype." <>
    txt (escape name)
generateDoc (ClassIr name args) = do
  tell $
    txt "function _" <>
    txt (escape name) <>
    txt "(" <>
    intercalate (txt ", ") (map txt args) <> 
    txt ") {" <>
    nest 2 (fold (map classArgDoc args)) <>
    line <>
    txt "};" <>
    line
  tell $
    txt "_" <>
    txt (escape name) <>
    txt ".prototype = Object.create(_Any.prototype);" <>
    line
  tell $
    txt "_Any.prototype.as" <>
    txt (escape name) <>
    txt " = function (_handle, f, cont) {" <> 
    nest 2 (
      line <>
      txt "f = f();" <>
      line <>
      txt "return cont(_handle)") <>
    line <>
    txt "};" <>
    line
  tell $
    txt "_" <>
    txt (escape name) <> 
    txt ".prototype.as" <>
    txt (escape name) <> 
    txt " = function (_handle, f, cont) {" <>
    nest 2 (
      line <>
      txt "f = f();" <>
      line <>
      txt "return f(_handle, " <>
      intercalate (txt ", ") (map (\n -> txt "() => this." <> txt n) args) <>
      txt ")") <>
    line <>
    txt "};" <>
    line
  tell $
    txt "function " <>
    txt (escape name) <>
    txt "(" <>
    intercalate (txt ", ") (txt "_handle" : map txt args) <> 
    txt ") { return new _" <> 
    txt (escape name) <>
    txt "(" <>
    intercalate (txt ", ") (map (\n -> txt n <> txt "()") args) <>
    txt "); };" <>
    line
  pure $ txt (escape name)

generate :: Int -> Ir -> String
generate width ir = 
  let (Tuple doc docs) = runWriter $ generateDoc ir
  in pretty width $ group docs <> group doc
