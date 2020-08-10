module Types where

import Prelude

import Data.Array (fromFoldable)
import Data.BigInt (BigInt, toString)
import Data.String (joinWith)
import Data.Tuple (Tuple)

data Expr
  = IdentExpr String
  | IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | OperatorExpr Expr (Array (Tuple String Expr))

instance showExpr :: Show Expr where
  show (IdentExpr name) = name
  show (IntExpr int) = toString int
  show (CharExpr char) = show char
  show (StringExpr string) = show string
  show (OperatorExpr expr operators) = show expr <> " " <> (joinWith " " $ fromFoldable $ map show operators)
