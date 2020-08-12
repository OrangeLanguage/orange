module Compiler where

import Prelude

import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.State (StateT, get, put)
import Data.BigInt (BigInt)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Types (Assoc(..), Expr(..), Ir(..), Op(..))

type Compiler a = ReaderT Env (StateT Int (Except String)) a

data Env = Env (Set String) (Map String Op)

insertGlobal :: String -> Env -> Env
insertGlobal name (Env globals ops) = Env (Set.insert name globals) ops

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env globals ops) = Env globals (Map.insert name (Op assoc prec expr) ops)

memberGlobal :: String -> Env -> Boolean
memberGlobal name (Env globals ops) = Set.member name globals

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env globals ops) = Map.lookup name ops

compileCont :: Partial => Expr -> (Ir -> Compiler Ir) -> Compiler Ir
compileCont (IdentExpr name) f = do
  env <- ask
  if memberGlobal name env
    then f $ IdentIr name
    else throwError $ "Undefined " <> name
compileCont (IntExpr int) f = f $ IntIr int
compileCont (CharExpr char) f = f $ CharIr char
compileCont (StringExpr string) f = f $ StringIr string
compileCont (ApplyExpr expr args) f = compileCont expr \e -> do
  i <- get
  put $ i + 1
  let name = show i
  cont <- f $ IdentIr name
  compileConts args \a -> pure $ ApplyIr e a name cont
compileCont (OpExpr expr operators) f = do
  env <- ask
  ops <- traverse (lookup env) operators
  let apply = applyOps ops Nil (expr : Nil)
  compileCont apply f
    where
      lookup env (Tuple name e) = case lookupOp name env of
        Nothing -> throwError $ "Undefined " <> name
        Just op -> pure $ Tuple op e
compileCont (DefExpr name expr) f = local (insertGlobal name) $ compileCont expr f
compileCont (InfixExpr assoc name prec expr) f = local (insertOp assoc name prec expr) $ compileCont expr f

compileConts :: Partial => List Expr -> (List Ir -> Compiler Ir) -> Compiler Ir
compileConts Nil f = f Nil
compileConts (car : cdr) f = compileCont car \carC -> compileConts cdr \c -> f (carC : c)

applyOps :: Partial => List (Tuple Op Expr) -> List Op -> List Expr -> Expr
applyOps Nil Nil (expr : Nil) = expr
applyOps Nil ((Op assoc proc expr) : ops) (right : left : exprs) = applyOps Nil ops ((ApplyExpr expr (left : right : Nil)) : exprs)
applyOps ((Tuple op expr) : rest) ops exprs = 
  let (Tuple ops' exprs') = runOp op ops exprs
  in applyOps rest ops' (expr : exprs')

runOp :: Partial => Op -> List Op -> List Expr -> (Tuple (List Op) (List Expr))
runOp op Nil exprs = Tuple (op : Nil) exprs
runOp op@(Op assoc opPrec expr) (pop@(Op _ popPrec _) : ops) (right : left : exprs) =  
  if popPrec > opPrec || (popPrec == opPrec && assoc == Left)
  then runOp op ops ((ApplyExpr expr (left : right : Nil)) : exprs)
  else Tuple (op : pop : ops) (left : right : exprs)
