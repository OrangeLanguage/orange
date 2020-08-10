module Types where

import Data.Show

import Data.BigInt (BigInt)

data Expr
  = IdentExpr String
  | IntExpr BigInt
  | CharExpr Char
  | StringExpr String

instance showExpr :: Show Expr where
  show (IdentExpr name) = name
  show (IntExpr int) = show int
  show (CharExpr char) = show char
  show (StringExpr string) = show string
