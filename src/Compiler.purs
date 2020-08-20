module Compiler where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (class MonadTrans, ExceptT(..), mapExceptT, runExceptT, throwError)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, execStateT, get, mapStateT, modify, put)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
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

derive instance newtypeCompilerT :: Newtype (CompilerT m a) _
derive newtype instance monadErrorCompilerT :: Monad m => MonadError String (CompilerT m)
derive newtype instance functorCompilerT :: Functor m => Functor (CompilerT m)
derive newtype instance applyCompilerT :: Monad m => Apply (CompilerT m)
derive newtype instance applicativeCompilerT :: Monad m => Applicative (CompilerT m)
derive newtype instance monadCompilerT :: Monad m => Monad (CompilerT m) 
derive newtype instance monadStateCompilerT :: Monad m => MonadState Env (CompilerT m)
derive newtype instance monadEffectCompilerT :: MonadEffect m => MonadEffect (CompilerT m)
derive newtype instance monadThrowCompilerT :: Monad m => MonadThrow String (CompilerT m)
derive newtype instance bindCompilerT :: Monad m => Bind (CompilerT m)

instance monadTransCompilerT :: MonadTrans CompilerT where
  lift ma = CompilerT $ StateT \s -> ExceptT $ ma <#> \a -> Right $ Tuple a s

mapCompilerT :: forall m n a. Monad m => Monad n => (forall b. m b -> n b) -> CompilerT m a -> CompilerT n a
mapCompilerT f (CompilerT c) = CompilerT $ mapStateT (mapExceptT f) c

runCompilerT :: forall m a. Monad m => CompilerT m a -> Env -> m (Either String Env)
runCompilerT compiler env = runExceptT $ execStateT (unwrap compiler) env

runCompiler :: forall a. Compiler a -> Env -> Either String Env
runCompiler compiler env = unwrap $ runCompilerT compiler env

evalCompilerT :: forall m a. Monad m => CompilerT m a -> Env -> m (Either String a)
evalCompilerT compiler env = runExceptT $ evalStateT (unwrap compiler) env

evalCompiler :: forall a. Compiler a -> Env -> Either String a
evalCompiler compiler env = unwrap $ evalCompilerT compiler env

data Env = Env Int (Map String Type) (Map String Type) (Map String Op)

insertDef :: String -> Type -> Env -> Env
insertDef name typ (Env i defs types ops) = Env i (insert name typ defs) types ops

insertType :: String -> Type -> Env -> Env
insertType name typ (Env i defs types ops) = Env i defs (insert name typ types) ops

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env i defs types ops) = Env i defs types (insert name (Op assoc prec expr) ops)

lookupDef :: String -> Env -> Maybe Type
lookupDef name (Env i defs types ops) = lookup name defs

lookupType :: String -> Env -> Maybe Type
lookupType name (Env i defs types ops) = lookup name types

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env i defs types ops) = lookup name ops

fresh :: forall m. Monad m => CompilerT m String
fresh = do
  (Env i defs types ops) <- get
  put $ Env (i + 1) defs types ops
  pure $ show i

check :: forall m. Monad m => Expr -> Type -> CompilerT m Ir
check (LambdaExpr args expr) typ = case typ of
  (FuncType retTyp argsTyp) -> do
    env <- get
    put $ foldr (uncurry insertDef) env $ zip args argsTyp
    exprIr <- check expr retTyp
    put env
    pure $ LambdaIr args exprIr
  x -> throwError $ "Expected function, found " <> showType 40 x
check expr typ = do
  (Tuple ir typ') <- synth expr
  checkIs typ typ'
  pure ir

checkIs :: forall m. Monad m => Type -> Type -> CompilerT m Unit
checkIs t1 (IdentType n2) = do
  env <- get
  case lookupType n2 env of 
    Nothing -> throwError $ "Undefined " <> n2
    Just t2 -> checkIs t1 t2
checkIs (IdentType n1) t2 = do
  env <- get
  case lookupType n1 env of 
    Nothing -> throwError $ "Undefined " <> n1
    Just t1 -> checkIs t1 t2
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
  case lookupDef name env of
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
    Nothing -> pure $ Tuple (IdentIr "unit") (IdentType "unit")
    Just (Tuple _ typ) -> pure $ Tuple (BlockIr (map fst synths)) typ
synth (DefExpr name (Just typ) expr) = do
  void $ modify $ insertDef name typ
  ir <- check expr typ
  pure $ Tuple (DefIr name ir) typ
synth (DefExpr name Nothing expr) = do
  (Tuple ir irTyp) <- synth expr
  void $ modify $ insertDef name irTyp
  pure $ Tuple (DefIr name ir) irTyp
synth (TypeExpr name typ) = do
  void $ modify $ insertType name typ
  pure $ Tuple (IdentIr "unit") (IdentType "unit")
synth (InfixExpr assoc name prec expr) = do
  void $ modify $ insertOp assoc name prec expr
  synth expr
synth (ExternExpr name typ) = do
  void $ modify $ insertDef name typ
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
