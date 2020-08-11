module Types where

import Prelude

import Data.Array (fromFoldable)
import Data.BigInt (BigInt, toString)
import Data.List (List)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

data Assoc = Left | Right

data Expr
  = IdentExpr String
  | IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | ApplyExpr Expr (List Expr)
  | OpExpr Expr (List (Tuple String Expr))
  | DefExpr String Expr
  | InfixExpr Assoc String BigInt Expr

instance showAssox :: Show Assoc where
  show Left = "left"
  show Right = "right"

instance showExpr :: Show Expr where
  show (IdentExpr name) = name
  show (IntExpr int) = toString int
  show (CharExpr char) = show char
  show (StringExpr string) = show string
  show (ApplyExpr expr args) = show expr <> "(" <> (joinWith ", " $ fromFoldable $ map show args) <> ")"
  show (OpExpr expr operators) = show expr <> " " <> (joinWith " " $ fromFoldable $ map showTuple operators)
  show (DefExpr name expr) = "def " <> name <> " = " <> show expr
  show (InfixExpr assoc op int expr) = "infix " <> show assoc <> " " <> op <> " " <> toString int <> " = " <> show expr

showTuple :: forall a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = show a <> " " <> show b
