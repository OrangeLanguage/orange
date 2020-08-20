module Pretty where

import Prelude

import Data.BigInt (BigInt, toString)
import Data.Foldable (intercalate)
import Data.List (fold)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, group, line, nest, pretty, text, txt)
import Types (Assoc(..), Expr(..), Ir(..))

assocDoc :: Assoc -> DOC  
assocDoc LeftAssoc = text "blue" "left "
assocDoc RightAssoc = text "blue" "right "

intDoc :: BigInt -> DOC
intDoc int = text "cyan" $ toString int

exprDoc :: Expr -> DOC
exprDoc (IntExpr int) = intDoc int
exprDoc (CharExpr char) = text "green" $ show char
exprDoc (StringExpr string) = text "green" $ show string
exprDoc (IdentExpr name) = txt name
exprDoc (ApplyExpr expr args) = 
  exprDoc expr <> 
  txt "(" <> 
  group (nest 2 $ intercalate (txt ", ") $ map (exprDoc >>> ((<>) line)) args) <> 
  txt ")"
exprDoc (OpExpr expr ops) = 
  exprDoc expr <> 
  fold (map (\(Tuple name e) -> txt (" " <> name <> " ") <> exprDoc e) ops)
exprDoc (BlockExpr exprs) = 
  txt "{" <>
  group (nest 2 (intercalate (txt ";") $ map (exprDoc >>> ((<>) line)) exprs) <> line) <>
  txt "}"
exprDoc (LambdaExpr args expr) = 
  txt "\\" <> 
  group (nest 2 $ intercalate (txt ", ") (map (txt >>> ((<>) line)) args)) <> 
  txt " -> " <> 
  group (nest 2 $ line <> exprDoc expr)
exprDoc (DoExpr expr) = 
  text "blue" "do " <> 
  exprDoc expr
exprDoc (HandleExpr expr cont) = 
  text "blue" "handle " <> 
  exprDoc expr <> 
  text "blue" " with " <> 
  line <> 
  exprDoc cont
exprDoc (DefExpr name expr) = 
  text "blue" "def " <> 
  txt name <>
  txt " = " <> 
  nest 2 (line <> exprDoc expr)
exprDoc (InfixExpr assoc op int expr) = 
  text "blue" "infix " <> 
  assocDoc assoc <> 
  intDoc int <> 
  txt (" " <> op <> " = ") <> 
  exprDoc expr
exprDoc (ExternExpr name) = 
  text "blue" "extern " <> 
  txt name
exprDoc (ClassExpr name args) =
  text "blue" "class " <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map txt args) <> 
  txt ")"

showExpr :: Int -> Expr -> String
showExpr width expr = pretty width $ group $ exprDoc expr

irDoc :: Ir -> DOC
irDoc (IntIr int) = intDoc int
irDoc (CharIr char) = text "green" $ show char
irDoc (StringIr string) = text "green" $ show string
irDoc (IdentIr name) = txt name
irDoc (ApplyIr ir args) = 
  irDoc ir <> 
  txt "(" <> 
  group (nest 2 $ intercalate (txt ", ") $ map (irDoc >>> ((<>) line)) args) <> 
  txt ")"
irDoc (BlockIr irs) = 
  txt "{" <>
  group (nest 2 (intercalate (txt ";") $ map (irDoc >>> ((<>) line)) irs) <> line) <>
  txt "}"
irDoc (LambdaIr args ir) = 
  txt "\\" <> 
  intercalate (txt ", ") (map txt args) <> 
  txt " -> " <> 
  group (nest 2 $ line <> irDoc ir)
irDoc (DoIr ir name cont) = 
  text "blue" "do " <> 
  txt (name <> " = ") <> 
  irDoc ir <> 
  text "blue" " in " <> 
  line <> 
  irDoc cont
irDoc (HandleIr ir cont) = 
  text "blue" "handle " <> 
  irDoc ir <> 
  text "blue" " with " <> 
  line <> 
  irDoc cont
irDoc (DefIr name ir) = 
  text "blue" "def " <> 
  txt name <>
  txt " = " <> 
  irDoc ir
irDoc (ClassIr name args) =
  text "blue" "class " <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map txt args) <> 
  txt ")"

showIr :: Int -> Ir -> String
showIr width ir = pretty width $ group $ irDoc ir
