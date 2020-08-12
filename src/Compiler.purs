module Compiler where

import Prelude

import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.State (StateT, get, put)
import Data.BigInt (BigInt)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Map (Map)
import Data.Set as Set
import Data.Set (Set)
import Types (Assoc, Expr(..), Ir(..))

type Compiler a = ReaderT Env (StateT Int (Except String)) a

data Op = Op Assoc BigInt

data Env = Env (Set String) (Map String Op)

insertGlobal :: String -> Env -> Env
insertGlobal name (Env globals ops) = Env (Set.insert name globals) ops

insertOp :: Assoc -> String -> BigInt -> Env -> Env
insertOp assoc name prec (Env globals ops) = Env globals (Map.insert name (Op assoc prec) ops)

memberGlobal :: String -> Env -> Boolean
memberGlobal name (Env globals ops) = Set.member name globals

compileCont :: Partial => Expr -> (Ir -> Compiler Ir) -> Compiler Ir
compileCont (IdentExpr name) f = do
  env <- ask
  if memberGlobal name env
    then f $ IdentIr name
    else throwError name
compileCont (IntExpr int) f = f $ IntIr int
compileCont (CharExpr char) f = f $ CharIr char
compileCont (StringExpr string) f = f $ StringIr string
compileCont (ApplyExpr expr args) f = compileCont expr \e -> do
  i <- get
  put $ i + 1
  let name = show i
  cont <- f $ IdentIr name
  compileConts args \a -> pure $ ApplyIr e a name cont
compileCont (DefExpr name expr) f = local (insertGlobal name) $ compileCont expr f
compileCont (InfixExpr assoc name prec expr) f = local (insertOp assoc name prec) $ compileCont expr f

compileConts :: Partial => List Expr -> (List Ir -> Compiler Ir) -> Compiler Ir
compileConts Nil f = f Nil
compileConts (car : cdr) f = compileCont car \carC -> compileConts cdr \c -> f (carC : c)
