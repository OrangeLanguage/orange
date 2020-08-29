module Generator where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (elem)
import Data.BigInt (toString)
import Data.List (List(..), fold, init, intercalate, last, (:))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, group, line, nest, nil, pretty, txt)
import Types (Ir(..))

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

classArgDoc :: String -> DOC
classArgDoc name = 
  line <>
  txt "this." <> 
  txt (escape name) <>
  txt " = " <>
  txt (escape name) <> 
  txt ";"

generateDoc :: Boolean -> Ir -> Writer DOC DOC
generateDoc _ (BoolIr bool) = pure $ txt $ if bool then "_true" else "_false"
generateDoc _ (IntIr int) = pure $ txt $ "new _Int(" <> toString int <> "n)"
generateDoc _ (CharIr char) = pure $ txt $ "new _Char(" <> show char <> ")"
generateDoc _ (StringIr string) = pure $ txt $ "new _String(" <> show string <> ")"
generateDoc _ (IdentIr name) = pure $ txt (escape name)
generateDoc _ (DotIr ir name) = do
  irDoc <- generateDoc false ir
  pure $ 
    txt "(" <> 
    irDoc <>
    txt "." <>
    txt (escape name) <>
    txt ")"
generateDoc _ (ApplyIr ir args) = do
  irDoc <- generateDoc false ir
  argsDoc <- traverse (generateDoc false) args
  pure $ 
    irDoc <> 
    txt "(" <> 
    intercalate (txt ", ") argsDoc <> 
    txt ")"
generateDoc captureThis (BlockIr (ir : Nil)) = generateDoc captureThis ir
generateDoc captureThis (BlockIr irs) = 
  let (Tuple irDocs docs) = runWriter $ traverse (generateDoc captureThis) irs
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
generateDoc captureThis (LambdaIr args ir) = do
  irDoc <- generateDoc false ir
  pure $ if captureThis
    then 
      txt "(function (" <> 
      intercalate (txt ", ") (map (escape >>> txt) args) <> 
      txt ") {" <>
      nest 2 (
        line <>
        txt "return " <> 
        irDoc <>
        txt ";") <>
      line <>
      txt "})"
    else
      txt "((" <> 
      intercalate (txt ", ") (map (escape >>> txt) args) <> 
      txt ") => " <>
      nest 2 (
        line <>
        irDoc) <>
      txt ")"
generateDoc _ (DoIr ir name cont) = do
  irDoc <- generateDoc false ir
  contDoc <- generateDoc false cont
  pure $ 
    txt "(() => { throw {effect: " <>
    irDoc <>
    txt ", resume: (" <> 
    txt name <>
    txt ", __cont) => " <>
    txt name <>
    txt "(" <>
    txt name <>
    txt " => __cont(" <>
    nest 2 contDoc <>
    txt "))}})()"
generateDoc _ (HandleIr ir cont) = do
  irDoc <- generateDoc false ir
  contDoc <- generateDoc false cont
  pure $ 
    txt "(() => { try {" <>
    nest 2 (
      line <>
      txt "return " <>
      irDoc <>
      txt ";") <>
    line <>
    txt "} catch (_e) {" <>
    nest 2 (line <> 
      txt "return " <>
      contDoc <>
      txt ";") <>
    line <>
    txt "}})()"
generateDoc _ (DefIr name ir) = do
  irDoc <- generateDoc false ir
  tell $
    txt "var " <>
    txt (escape name) <>
    txt " = " <>
    irDoc <>
    txt ";" <> 
    line
  pure $ txt (escape name)
generateDoc _ (ExtendIr clazz name ir) = do
  irDoc <- generateDoc true ir
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
generateDoc _ (ClassIr name args) = do
  tell $
    txt "function _" <>
    txt (escape name) <>
    txt "(" <>
    intercalate (txt ", ") (map (escape >>> txt) args) <> 
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
  pure $ txt "_unit"
generateDoc _ (ExternIr string) = pure $ txt string

generate :: Int -> Ir -> String
generate width ir = 
  let (Tuple doc docs) = runWriter $ generateDoc false ir
  in pretty width $ group docs <> group doc
