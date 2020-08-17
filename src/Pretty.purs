module Pretty where

import Prelude

import Data.BigInt (BigInt, toString)
import Data.Foldable (intercalate)
import Data.List (fold)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Prettier.Printer (DOC, group, line, nest, pretty, text, txt)
import Types (Assoc(..), Expr(..), Ir(..), Type(..))

assocDoc :: Assoc -> DOC
assocDoc LeftAssoc = text "blue" "left "
assocDoc RightAssoc = text "blue" "right "

intDoc :: BigInt -> DOC
intDoc int = text "cyan" $ toString int

typeDoc :: Type -> DOC
typeDoc (IdentType typ) = txt typ
typeDoc (ApplyType typ args) = typeDoc typ <> txt "<" <> (group $ nest 2 $ intercalate (txt ", ") $ map (typeDoc >>> ((<>) line)) args) <> txt ">"
typeDoc (FuncType return args) = txt "(" <> (group $ nest 2 $ intercalate (txt ", ") $ map (typeDoc >>> ((<>) line)) args) <> txt ") -> " <> line <> typeDoc return

showType :: Int -> Type -> String
showType width typ = pretty width $ group $ typeDoc typ

withTypeDoc :: String -> Maybe Type -> DOC
withTypeDoc name Nothing = txt name
withTypeDoc name (Just typ) = txt name <> group (nest 2 $ line <> txt ": " <> typeDoc typ)

exprDoc :: Expr -> DOC
exprDoc (IdentExpr name) = txt name
exprDoc (IntExpr int) = intDoc int
exprDoc (CharExpr char) = text "green" $ show char
exprDoc (StringExpr string) = text "green" $ show string
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
  group (nest 2 $ intercalate (txt "; ") $ map (exprDoc >>> ((<>) line)) exprs) <> 
  line <> 
  txt "}"
exprDoc (LambdaExpr args expr) = 
  txt "\\" <> 
  group (nest 2 $ intercalate (txt ", ") (map (uncurry withTypeDoc >>> ((<>) line)) args)) <> 
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
exprDoc (DefExpr name typ expr) = 
  text "blue" "def " <> 
  withTypeDoc name typ <> 
  txt " = " <> 
  nest 2 (line <> exprDoc expr)
exprDoc (TypeExpr name typ) = 
  text "blue" "def " <> 
  txt "name" <>
  txt " = " <> 
  nest 2 (line <> typeDoc typ)
exprDoc (InfixExpr assoc op int expr) = 
  text "blue" "infix " <> 
  assocDoc assoc <> 
  intDoc int <> 
  txt (" " <> op <> " = ") <> 
  exprDoc expr
exprDoc (ExternExpr name typ) = 
  text "blue" "extern " <> 
  txt name <> 
  txt " : " <> 
  nest 2 (line <> typeDoc typ)

showExpr :: Int -> Expr -> String
showExpr width expr = pretty width $ group $ exprDoc expr

irDoc :: Ir -> DOC
irDoc (IdentIr name) = txt name
irDoc (IntIr int) = intDoc int
irDoc (CharIr char) = text "green" $ show char
irDoc (StringIr string) = text "green" $ show string
irDoc (ApplyIr ir args) = irDoc ir <> txt "(" <> (group $ nest 2 $ intercalate (txt ", ") $ map (irDoc >>> ((<>) line)) args) <> txt ")"
irDoc (BlockIr irs) = txt "{" <> (group $ nest 2 $ intercalate (txt "; ") $ map (irDoc >>> ((<>) line)) irs) <> txt "}"
irDoc (LambdaIr args ir) = txt "\\" <> intercalate (txt ", ") (map txt args) <> txt " -> " <> (group $ nest 2 $ line <> irDoc ir)
irDoc (DoIr ir) = text "blue" "do " <> irDoc ir
irDoc (HandleIr ir cont) = text "blue" "handle " <> irDoc ir <> text "blue" " with " <> line <> irDoc cont
irDoc (DefIr name ir) = text "blue" "def " <> txt (name <> " = ") <> irDoc ir

showIr :: Int -> Ir -> String
showIr width ir = pretty width $ group $ irDoc ir
