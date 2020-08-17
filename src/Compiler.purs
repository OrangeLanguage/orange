module Compiler where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, modify, put)
import Data.BigInt (BigInt)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Identity (Identity)
import Data.List (List(..), last, zip, (:))
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect.Class (class MonadEffect)
import Pretty (showExpr, showType)
import Types (Assoc(..), Expr(..), Ir(..), Op(..), Type(..))

newtype CompilerT m a = CompilerT (StateT Env (ExceptT String m) a)
type Compiler a = CompilerT Identity a

derive instance newtypeCompiler :: Newtype (CompilerT m a) _
derive newtype instance monadErrorCompiler :: Monad m => MonadError String (CompilerT m)
derive newtype instance functorCompiler :: Functor m => Functor (CompilerT m)
derive newtype instance applyCompiler :: Monad m => Apply (CompilerT m)
derive newtype instance applicativeCompiler :: Monad m => Applicative (CompilerT m)
derive newtype instance monadCompiler :: Monad m => Monad (CompilerT m) 
derive newtype instance monadStateCompiler :: Monad m => MonadState Env (CompilerT m)
derive newtype instance monadEffectCompiler :: MonadEffect m => MonadEffect (CompilerT m)
derive newtype instance monadThrowCompiler :: Monad m => MonadThrow String (CompilerT m)
derive newtype instance bindCompiler :: Monad m => Bind (CompilerT m)

runCompilerT :: forall m a. Monad m => CompilerT m a -> Env -> m (Either String a)
runCompilerT compiler env = runExceptT $ evalStateT (unwrap compiler) env

runCompiler :: forall a. Compiler a -> Env -> Either String a
runCompiler compiler env = unwrap $ runCompilerT compiler env

data Env = Env Int (Map String Type) (Map String Op)

insertGlobal :: String -> Type -> Env -> Env
insertGlobal name typ (Env i globals ops) = Env i (insert name typ globals) ops

insertGlobals :: List (Tuple String Type) -> Env -> Env
insertGlobals names env = foldr (uncurry insertGlobal) env names

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env i globals ops) = Env i globals (insert name (Op assoc prec expr) ops)

lookupGlobal :: String -> Env -> Maybe Type
lookupGlobal name (Env i globals ops) = lookup name globals

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env i globals ops) = lookup name ops

fresh :: forall m. Monad m => CompilerT m String
fresh = do
  (Env i globals ops) <- get
  put $ Env (i + 1) globals ops
  pure $ show i

check :: forall m. Monad m => Expr -> Type -> CompilerT m Ir
check (LambdaExpr args expr) typ = case typ of
  (FuncType retTyp argsTyp) -> do
    env <- get
    void $ modify $ insertGlobals (zip (map fst args) argsTyp)
    exprIr <- check expr retTyp
    put env
    pure $ LambdaIr (map fst args) exprIr
  x -> throwError $ "Expected function, found " <> showType 40 x
check expr typ = do
  (Tuple ir typ') <- synth expr
  checkIs typ typ'
  pure ir

checkIs :: forall m. Monad m => Type -> Type -> CompilerT m Unit
checkIs (IdentType n1) (IdentType n2) = if n1 == n2 
  then pure unit
  else throwError $ n1 <> " is not a " <> n2
checkIs (FuncType r1 args1) (FuncType r2 args2) = do
  checkIs r1 r2
  checkIsList args1 args2
checkIs t1 t2 = throwError $ showType 40 t1 <> " is not a " <> showType 40 t2

checkIsList :: forall m. Monad m => List Type -> List Type -> CompilerT m Unit
checkIsList Nil Nil = pure unit
checkIsList (car : cdr) (car' : cdr') = do
  checkIs car car'
  checkIsList cdr cdr'
checkIsList l1 l2 = throwError "Mismatched lists"

synth :: forall m. Monad m => Expr -> CompilerT m (Tuple Ir Type)
synth (IdentExpr name) = do
  env <- get
  case lookupGlobal name env of
    Nothing -> throwError $ "Undefined " <> name
    Just typ -> pure $ Tuple (IdentIr name) typ
synth (IntExpr int) = pure $ Tuple (IntIr int) $ IdentType "int"
synth (CharExpr char) = pure $ Tuple (CharIr char) $ IdentType "char"
synth (StringExpr string) = pure $ Tuple (StringIr string) $ IdentType "string"
synth (ApplyExpr fun args) = do
  (Tuple funIr funTyp) <- synth fun
  case funTyp of 
    (FuncType retTyp argsTyp) -> do
      argsIrs <- traverse (\(Tuple expr typ) -> check expr typ) $ zip args argsTyp
      pure $ Tuple (ApplyIr funIr argsIrs) retTyp
    x -> throwError $ "Expected function, found " <> showType 40 x
synth (OpExpr expr operators) = do
  env <- get
  ops <- traverse (\(Tuple name e) -> case lookupOp name env of
    Nothing -> throwError $ "Undefined " <> name
    Just op -> pure $ Tuple op e) operators
  apply <- applyOps ops Nil (expr : Nil)
  synth apply
synth (BlockExpr exprs) = do
  synths <- traverse synth exprs
  case last synths of
    Nothing -> throwError $ "Empty block"
    Just (Tuple _ typ) -> pure $ Tuple (BlockIr (map fst synths)) typ
synth (DefExpr name (Just typ) expr) = do
  void $ modify $ insertGlobal name typ
  ir <- check expr typ
  pure $ Tuple (DefIr name ir) typ
synth (DefExpr name Nothing expr) = do
  (Tuple ir irTyp) <- synth expr
  void $ modify $ insertGlobal name irTyp
  pure $ Tuple (DefIr name ir) irTyp
synth (InfixExpr assoc name prec expr) = do
  void $ modify $ insertOp assoc name prec expr
  synth expr
synth (ExternExpr name typ) = do
  void $ modify $ insertGlobal name typ
  pure $ Tuple (IdentIr name) typ
synth expr = throwError $ "Could not synthesize type for " <> showExpr 40 expr

applyOps :: forall m. (MonadError String m) => List (Tuple Op Expr) -> List Op -> List Expr -> m Expr
applyOps Nil Nil (expr : Nil) = pure expr
applyOps Nil ((Op assoc proc expr) : ops) (right : left : exprs) = applyOps Nil ops ((ApplyExpr expr (left : right : Nil)) : exprs)
applyOps ((Tuple op expr) : rest) ops exprs = do 
  (Tuple ops' exprs') <- runOp op ops exprs
  applyOps rest ops' (expr : exprs')
applyOps _ _ _ = throwError "Invalid operator state" 

runOp :: forall m. (MonadError String m) => Op -> List Op -> List Expr -> m (Tuple (List Op) (List Expr))
runOp op Nil exprs = pure $ Tuple (op : Nil) exprs
runOp op@(Op assoc opPrec expr) (pop@(Op _ popPrec _) : ops) (right : left : exprs) =  
  if popPrec > opPrec || (popPrec == opPrec && assoc == LeftAssoc)
  then runOp op ops ((ApplyExpr expr (left : right : Nil)) : exprs)
  else pure $ Tuple (op : pop : ops) (left : right : exprs)
runOp _ _ _ = throwError "Invalid operator state" 
