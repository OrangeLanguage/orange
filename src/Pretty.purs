module Pretty where

import Prelude

import Chalk (blue, cyan, green, yellow)
import Data.BigInt (BigInt, toString)
import Data.Foldable (intercalate)
import Data.List (fold)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, line, pretty, text)
import Types (Assoc(..), Expr(..), Ir(..))

assocDoc :: Assoc -> DOC
assocDoc Left = text $ blue "left "
assocDoc Right = text $ blue "right "

intDoc :: BigInt -> DOC
intDoc int = text $ cyan $ toString int

exprDoc :: Expr -> DOC
exprDoc (IdentExpr name) = text $ name
exprDoc (IntExpr int) = intDoc int
exprDoc (CharExpr char) = text $ green $ show char
exprDoc (StringExpr string) = text $ green $ show string
exprDoc (ApplyExpr expr args) = exprDoc expr <> text "(" <> (intercalate (text ", ") $ map exprDoc args) <> text ")"
exprDoc (OpExpr expr ops) = exprDoc expr <> (fold $ map (\(Tuple name e) -> text (" " <> name <> " ") <> exprDoc e) ops)
exprDoc (DefExpr name expr) = text (blue "def ") <> text (name <> " = ") <> exprDoc expr
exprDoc (InfixExpr assoc op int expr) = text (blue "infix ") <> assocDoc assoc <> intDoc int <> text (" " <> op <> " = ") <> exprDoc expr
exprDoc (ExternExpr name) = text (blue "extern ") <> text name

showExpr :: Int -> Expr -> String
showExpr width expr = pretty width $ exprDoc expr

irDoc :: Ir -> DOC
irDoc (IdentIr name) = text name
irDoc (IntIr int) = intDoc int
irDoc (CharIr char) = text $ green $ show char
irDoc (StringIr string) = text $ green $ show string
irDoc (ApplyIr ir args name cont) = irDoc ir <> text "(" <> (intercalate (text ", ") $ map irDoc args) <> text ") " <> text (yellow name) <> text " -> " <> line <> irDoc cont

showIr :: Int -> Ir -> String
showIr width ir = pretty width $ irDoc ir
