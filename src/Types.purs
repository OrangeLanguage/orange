module Types where

import Prelude

import Data.Array (fromFoldable)
import Data.BigInt (BigInt, toString)
import Data.List (List)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

data Assoc = Left | Right

data Op = Op Assoc BigInt Expr

data Expr
  = IdentExpr String
  | IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | ApplyExpr Expr (List Expr)
  | OpExpr Expr (List (Tuple String Expr))
  | DefExpr String Expr
  | InfixExpr Assoc String BigInt Expr

data Ir
  = IdentIr String
  | IntIr BigInt
  | CharIr Char
  | StringIr String
  | ApplyIr Ir (List Ir) String Ir

derive instance eqAssoc :: Eq Assoc
instance showAssox :: Show Assoc where
  show Left = "left"
  show Right = "right"

instance showOp :: Show Op where
  show (Op assoc prec expr) = "Op " <> show assoc <> " " <> toString prec <> " " <> show expr

instance showExpr :: Show Expr where
  show (IdentExpr name) = name
  show (IntExpr int) = toString int
  show (CharExpr char) = show char
  show (StringExpr string) = show string
  show (ApplyExpr expr args) = show expr <> "(" <> (joinWith ", " $ fromFoldable $ map show args) <> ")"
  show (OpExpr expr operators) = show expr <> " " <> (joinWith " " $ fromFoldable $ map showTuple operators)
  show (DefExpr name expr) = "def " <> name <> " = " <> show expr
  show (InfixExpr assoc op int expr) = "infix " <> show assoc <> " " <> op <> " " <> toString int <> " = " <> show expr

instance showIr :: Show Ir where
  show (IdentIr name) = name
  show (IntIr int) = toString int
  show (CharIr char) = show char
  show (StringIr string) = show string
  show (ApplyIr expr args name cont) = show expr <> "(" <> (joinWith " " $ fromFoldable $ map show args) <> ") " <> name <> " -> " <> show cont

showTuple :: forall a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = show a <> " " <> show b
