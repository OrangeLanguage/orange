module Types where

import Prelude

import Data.Array (fromFoldable)
import Data.BigInt (BigInt, toString)
import Data.List (List)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

data Expr
  = IdentExpr String
  | IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | ApplyExpr Expr (List Expr)
  | OperatorExpr Expr (List (Tuple String Expr))

instance showExpr :: Show Expr where
  show (IdentExpr name) = name
  show (IntExpr int) = toString int
  show (CharExpr char) = show char
  show (StringExpr string) = show string
  show (ApplyExpr expr args) = show expr <> "(" <> (joinWith ", " $ fromFoldable $ map show args) <> ")"
  show (OperatorExpr expr operators) = show expr <> " " <> (joinWith " " $ fromFoldable $ map showTuple operators)

showTuple :: forall a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = show a <> " " <> show b
