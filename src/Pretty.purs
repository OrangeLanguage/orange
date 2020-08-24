module Pretty where

import Prelude

import Data.BigInt (BigInt, toString)
import Data.Foldable (intercalate)
import Data.List (fold)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), uncurry)
import Prettier.Printer (DOC, group, line, nest, nil, pretty, text, txt)
import Types (Assoc(..), Eval(..), Expr(..), Ir(..))

assocDoc :: Assoc -> DOC  
assocDoc LeftAssoc = text "blue" "left "
assocDoc RightAssoc = text "blue" "right "

evalDoc :: Eval -> DOC
evalDoc EagerEval = nil
evalDoc LazyEval = text "blue" "lazy "

intDoc :: BigInt -> DOC
intDoc int = text "cyan" $ toString int

argDoc :: Eval -> String -> DOC
argDoc eval name = evalDoc eval <> txt name

exprDoc :: Expr -> DOC
exprDoc (IntExpr int) = intDoc int
exprDoc (CharExpr char) = text "green" $ show char
exprDoc (StringExpr string) = text "green" $ show string
exprDoc (IdentExpr name) = txt name
exprDoc (DotExpr expr name) =
  exprDoc expr <>
  txt "." <>
  txt name
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
  group (nest 2 $ intercalate (txt ", ") (map (uncurry argDoc) args)) <> 
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
exprDoc (DefExpr clazz name expr) = 
  text "blue" "def " <> 
  maybe nil txt clazz <>
  txt name <>
  txt " = " <> 
  nest 2 (line <> exprDoc expr)
exprDoc (InfixExpr assoc op int expr) = 
  text "blue" "infix " <> 
  assocDoc assoc <> 
  intDoc int <> 
  txt (" " <> op <> " = ") <> 
  exprDoc expr
exprDoc (ClassExpr name args) =
  text "blue" "class " <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map txt args) <> 
  txt ")"
exprDoc (WithExpr name) =
  text "blue" "with " <>
  txt name
exprDoc (ExternExpr name) = 
  text "blue" "extern " <> 
  txt name

showExpr :: Int -> Expr -> String
showExpr width expr = pretty width $ group $ exprDoc expr

irDoc :: Ir -> DOC
irDoc (IntIr int) = intDoc int
irDoc (CharIr char) = text "green" $ show char
irDoc (StringIr string) = text "green" $ show string
irDoc (IdentIr name) = txt name
irDoc (DotIr ir name) =
  txt "(" <>
  irDoc ir <>
  txt "." <>
  txt name <>
  txt ")"
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
  intercalate (txt ", ") (map (uncurry argDoc) args) <> 
  txt " -> " <> 
  txt "(" <>
  group (nest 2 $ line <> irDoc ir) <>
  txt ")"
irDoc (DoIr ir name cont) = 
  text "blue" "do " <> 
  txt (name <> " = ") <> 
  irDoc ir <> 
  text "blue" " in " <> 
  line <> 
  txt "(" <>
  irDoc cont <>
  txt ")"
irDoc (HandleIr ir cont) = 
  text "blue" "handle " <> 
  irDoc ir <> 
  text "blue" " with " <> 
  line <> 
  txt "(" <>
  irDoc cont <>
  txt ")"
irDoc (DefIr name ir) = 
  text "blue" "def " <> 
  txt name <>
  txt " = " <> 
  txt "(" <>
  irDoc ir <>
  txt ")"
irDoc (ExtendIr clazz name ir) = 
  text "blue" "extend " <> 
  txt clazz <>
  txt " " <>
  txt name <>
  txt " = " <> 
  txt "(" <>
  irDoc ir <>
  txt ")"
irDoc (ClassIr name args) =
  text "blue" "class " <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map txt args) <> 
  txt ")"
irDoc (WithIr name) =
  text "blue" "with " <>
  txt name

showIr :: Int -> Ir -> String
showIr width ir = pretty width $ group $ irDoc ir
