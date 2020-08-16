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
  | LambdaExpr (List (Tuple String Type)) Expr
  | DoExpr Expr
  | HandleExpr Expr Expr
  | DefExpr String Type Expr
  | InfixExpr Assoc String BigInt Expr
  | ExternExpr String Type

data Type
  = AnyType
  | IdentType String
  | ApplyType Type (List Type)
  | FuncType Type (List Type)

data Ir
  = IdentIr String
  | IntIr BigInt
  | CharIr Char
  | StringIr String
  | ApplyIr Ir (List Ir)
  | LambdaIr (List String) Ir
  | DoIr Ir String Ir
  | HandleIr Ir Ir
  | DefIr String Ir Ir

derive instance eqAssoc :: Eq Assoc

showTuple :: forall a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = show a <> " " <> show b
