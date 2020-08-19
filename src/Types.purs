module Types where

import Prelude

import Data.BigInt (BigInt)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

data Assoc = LeftAssoc | RightAssoc

data Op = Op Assoc BigInt Expr

data Expr
  = IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | IdentExpr String
  | ApplyExpr Expr (List Expr)
  | OpExpr Expr (List (Tuple String Expr))
  | BlockExpr (List Expr)
  | LambdaExpr (List String) Expr
  | DoExpr Expr
  | HandleExpr Expr Expr
  | DefExpr String (Maybe Type) Expr
  | TypeExpr String Type
  | InfixExpr Assoc String BigInt Expr
  | ExternExpr String Type

data Type
  = IntType
  | CharType
  | StringType
  | UnitType
  | IdentType String
  | ApplyType Type (List Type)
  | FuncType Type (List Type)

data Ir
  = IntIr BigInt
  | CharIr Char
  | StringIr String
  | IdentIr String
  | ApplyIr Ir (List Ir)
  | BlockIr (List Ir)
  | LambdaIr (List String) Ir
  | DoIr Ir
  | HandleIr Ir Ir
  | DefIr String Ir

derive instance eqAssoc :: Eq Assoc
