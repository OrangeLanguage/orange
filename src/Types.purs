module Types where

import Prelude

import Data.BigInt (BigInt)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

data Assoc = LeftAssoc | RightAssoc
data Eval = EagerEval | LazyEval

data Op = Op Assoc BigInt Expr

data Expr
  = IntExpr BigInt
  | CharExpr Char
  | StringExpr String
  | IdentExpr String
  | DotExpr Expr String
  | ApplyExpr Expr (List Expr)
  | OpExpr Expr (List (Tuple String Expr))
  | BlockExpr (List Expr)
  | LambdaExpr (List (Tuple Eval String)) Expr
  | DoExpr Expr
  | HandleExpr Expr Expr
  | DefExpr (Maybe String) String Expr
  | InfixExpr Assoc String BigInt Expr
  | ClassExpr String (List String)
  | MixinExpr String
  | ExternExpr String

data Ir
  = IntIr BigInt
  | CharIr Char
  | StringIr String
  | IdentIr String
  | DotIr Ir String
  | ApplyIr Ir (List Ir)
  | BlockIr (List Ir)
  | LambdaIr (List (Tuple Eval String)) Ir
  | DoIr Ir String Ir
  | HandleIr Ir Ir
  | DefIr String Ir
  | ExtendIr String String Ir
  | ClassIr String (List String)
  | MixinIr String

derive instance eqAssoc :: Eq Assoc
