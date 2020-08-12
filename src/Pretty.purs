module Pretty where

import Prelude

import Data.BigInt (BigInt, toString)
import Data.Foldable (intercalate)
import Data.List (fold)
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, group, line, nest, pretty, text)
import Types (Assoc(..), Expr(..), Ir(..))

txt :: String -> DOC
txt = text ""

assocDoc :: Assoc -> DOC
assocDoc Left = text "blue" "left "
assocDoc Right = text "blue" "right "

intDoc :: BigInt -> DOC
intDoc int = text "cyan" $ toString int

exprDoc :: Expr -> DOC
exprDoc (IdentExpr name) = txt name
exprDoc (IntExpr int) = intDoc int
exprDoc (CharExpr char) = text "green" $ show char
exprDoc (StringExpr string) = text "green" $ show string
exprDoc (ApplyExpr expr args) = exprDoc expr <> txt "(" <> (group $ nest 2 $ intercalate (txt "," <> line) $ map exprDoc args) <> txt ")"
exprDoc (OpExpr expr ops) = exprDoc expr <> (fold $ map (\(Tuple name e) -> txt (" " <> name <> " ") <> exprDoc e) ops)
exprDoc (DefExpr name expr) = text "blue" "def " <> txt (name <> " = ") <> exprDoc expr
exprDoc (InfixExpr assoc op int expr) = text "blue" "infix " <> assocDoc assoc <> intDoc int <> txt (" " <> op <> " = ") <> exprDoc expr
exprDoc (ExternExpr name) = text "blue" "extern " <> txt name

showExpr :: Int -> Expr -> String
showExpr width expr = pretty width $ exprDoc expr

irDoc :: Ir -> DOC
irDoc (IdentIr name) = txt name
irDoc (IntIr int) = intDoc int
irDoc (CharIr char) = text "green" $ show char
irDoc (StringIr string) = text "green" $ show string
irDoc (ApplyIr ir args name cont) = irDoc ir <> txt "(" <> (group $ nest 2 $ intercalate (txt ", ") $ map irDoc args) <> txt ") " <> text "yellow" name <> txt " ->" <> line <> irDoc cont

showIr :: Int -> Ir -> String
showIr width ir = pretty width $ group $ irDoc ir
