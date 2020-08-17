module Types where

import Prelude

import Data.BigInt (BigInt)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

data Assoc = LeftAssoc | RightAssoc

data Op = Op Assoc BigInt Expr

data Expr
  = IdentExpr String
  | IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | ApplyExpr Expr (List Expr)
  | OpExpr Expr (List (Tuple String Expr))
  | BlockExpr (List Expr)
  | LambdaExpr (List (Tuple String (Maybe Type))) Expr
  | DoExpr Expr
  | HandleExpr Expr Expr
  | DefExpr String (Maybe Type) Expr
  | InfixExpr Assoc String BigInt Expr
  | ExternExpr String Type

data Type
  = IdentType String
  | ApplyType Type (List Type)
  | FuncType Type (List Type)

data Ir
  = IdentIr String
  | IntIr BigInt
  | CharIr Char
  | StringIr String
  | ApplyIr Ir (List Ir)
  | BlockIr (List Ir)
  | LambdaIr (List String) Ir
  | DoIr Ir
  | HandleIr Ir Ir
  | DefIr String Ir

derive instance eqAssoc :: Eq Assoc
