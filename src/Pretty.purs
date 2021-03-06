module Pretty where

import Prelude

import Data.BigInt (BigInt, toString)
import Data.Foldable (intercalate)
import Data.List (fold)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, group, line, nest, nil, pretty, text, txt)
import Types (Arg(..), Assoc(..), Eval(..), Expr(..), Ir(..), Pattern(..))

assocDoc :: Assoc -> DOC  
assocDoc LeftAssoc = text "blue" "left "
assocDoc RightAssoc = text "blue" "right "

evalDoc :: Eval -> DOC
evalDoc EagerEval = nil
evalDoc LazyEval = text "blue" "lazy "

intDoc :: BigInt -> DOC
intDoc int = text "cyan" $ toString int

argDoc :: Arg -> DOC
argDoc (Arg eval name) = evalDoc eval <> txt name

patternDoc :: Pattern -> DOC
patternDoc (Pattern name args expr) =
  line <>
  txt name <>
  txt "(" <>
  intercalate (txt ", ") (map argDoc args) <>
  txt ") " <>
  exprDoc expr

exprDoc :: Expr -> DOC
exprDoc (BoolExpr bool) = if bool then text "blue" "true" else text "blue" "false"
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
  group (nest 2 $ intercalate (txt ", ") (map argDoc args)) <> 
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
exprDoc (MatchExpr expr patterns) = 
  text "blue" "match " <>
  exprDoc expr <>
  group (nest 2 $ fold (map patternDoc patterns))
exprDoc (DefExpr clazz name expr) = 
  text "blue" "def " <> 
  maybe nil txt clazz <>
  txt name <>
  txt " = " <> 
  nest 2 (line <> exprDoc expr)
exprDoc (LetExpr name expr) = 
  text "blue" "let " <> 
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
  intercalate (txt ", ") (map argDoc args) <> 
  txt ")"
exprDoc (ImportExpr name) =
  text "blue" "import " <>
  exprDoc (StringExpr name)
exprDoc (ExternExpr string) =
  text "blue" "extern " <>
  text "green" (show string)

showExpr :: Int -> Expr -> String
showExpr width expr = pretty width $ group $ exprDoc expr

irDoc :: Ir -> DOC
irDoc (BoolIr bool) = if bool then text "blue" "true" else text "blue" "false"
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
  nest 2 (intercalate (txt ", ") $ map irDoc args) <> 
  txt ")"
irDoc (BlockIr irs) = 
  txt "{" <>
  nest 2 (intercalate (txt ";") $ map (irDoc >>> ((<>) line)) irs) <> line <>
  txt "}"
irDoc (LambdaIr args ir) = 
  txt "\\" <> 
  intercalate (txt ", ") (map txt args) <> 
  txt " -> " <> 
  txt "(" <>
  nest 2 (line <> irDoc ir) <>
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
irDoc (ExternIr string) =
  text "blue" "extern " <>
  text "green" (show string)

showIr :: Int -> Ir -> String
showIr width ir = pretty width $ group $ irDoc ir
