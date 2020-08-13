module Types where

import Prelude

import Data.BigInt (BigInt)
import Data.List (List)
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
  | BlockExpr (List Expr)
  | DefExpr String Expr
  | InfixExpr Assoc String BigInt Expr
  | ExternExpr String

data Ir
  = IdentIr String
  | IntIr BigInt
  | CharIr Char
  | StringIr String
  | DefIr String Ir Ir 
  | ApplyIr Ir (List Ir) String Ir

derive instance eqAssoc :: Eq Assoc

showTuple :: forall a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = show a <> " " <> show b
