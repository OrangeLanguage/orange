module Generator where

import Prelude

import Data.BigInt (toString)
import Data.List (intercalate, (:))
import Prettier.Printer (DOC, group, line, nest, pretty, txt)
import Types (Ir(..))

generateDoc :: Ir -> DOC
generateDoc (IdentIr name) = txt name
generateDoc (IntIr int) = txt $ toString int
generateDoc (CharIr char) = txt $ show char
generateDoc (StringIr string) = txt $ show string
generateDoc (ApplyIr ir args) =
  generateDoc ir <> 
  txt "(" <> 
  intercalate (txt ", ") (txt "_handle" : (map generateDoc args)) <> 
  txt ")"
generateDoc (LambdaIr args ir) = 
  txt "function _(" <> 
  intercalate (txt ", ") (txt "_handle" : (map txt args)) <> 
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
generateDoc (DefIr name ir cont) =
  txt "function _() {" <>
  nest 2 (
    line <>
    txt "const " <>
    txt name <>
    txt " = " <>
    generateDoc ir <>
    txt ";" <>
    line <>
    txt "return " <>
    generateDoc cont <>
    txt ";") <>
  line <>
  txt "}()"

generate :: Int -> Ir -> String
generate width ir = pretty width $ group $ generateDoc ir
