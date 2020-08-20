module Compiler where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (class MonadTrans, ExceptT(..), mapExceptT, runExceptT, throwError)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, execStateT, get, mapStateT, modify, put)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Types (Assoc(..), Expr(..), Ir(..), Op(..))

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

transformCompilerT :: forall m n a. Monad m => Monad n => (forall b. m b -> n b) -> CompilerT m a -> CompilerT n a
transformCompilerT f (CompilerT c) = CompilerT $ mapStateT (mapExceptT f) c

runCompilerT :: forall m a. Monad m => CompilerT m a -> Env -> m (Either String Env)
runCompilerT compiler env = runExceptT $ execStateT (unwrap compiler) env

runCompiler :: forall a. Compiler a -> Env -> Either String Env
runCompiler compiler env = unwrap $ runCompilerT compiler env

evalCompilerT :: forall m a. Monad m => CompilerT m a -> Env -> m (Either String a)
evalCompilerT compiler env = runExceptT $ evalStateT (unwrap compiler) env

evalCompiler :: forall a. Compiler a -> Env -> Either String a
evalCompiler compiler env = unwrap $ evalCompilerT compiler env

data Env = Env Int (Map String Unit) (Map String Op)

insertDef :: String -> Env -> Env
insertDef name (Env i defs ops) = Env i (insert name unit defs) ops

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env i defs ops) = Env i defs (insert name (Op assoc prec expr) ops)

lookupDef :: String -> Env -> Maybe Unit
lookupDef name (Env i defs ops) = lookup name defs

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env i defs ops) = lookup name ops

fresh :: forall m. Monad m => CompilerT m String
fresh = do
  (Env i defs ops) <- get
  put $ Env (i + 1) defs ops
  pure $ show i

compile :: forall m. Monad m => Expr -> CompilerT m Ir
compile (IntExpr int) = pure $ IntIr int
compile (CharExpr char) = pure $ CharIr char
compile (StringExpr string) = pure $ StringIr string
compile (IdentExpr name) = do
  env <- get
  case lookupDef name env of
    Nothing -> throwError $ "Undefined " <> name
    Just unit -> pure $ IdentIr name
compile (ApplyExpr fun args) = do
  funIr <- compile fun
  argsIrs <- traverse compile args
  pure $ ApplyIr funIr argsIrs
compile (OpExpr expr operators) = do
  env <- get
  ops <- traverse (\(Tuple name e) -> case lookupOp name env of
    Nothing -> throwError $ "Undefined " <> name
    Just op -> pure $ Tuple op e) operators
  apply <- applyOps ops Nil (expr : Nil)
  compile apply
compile (BlockExpr exprs) = do
  compiles <- traverse compile exprs
  pure $ BlockIr compiles
compile (LambdaExpr args expr) = do
  env <- get
  put $ foldr insertDef env args
  exprIr <- compile expr
  put env
  pure $ LambdaIr args exprIr
compile (DoExpr expr) = do
  ir <- compile expr
  pure $ DoIr ir
compile (HandleExpr expr cont) = do
  ir <- compile expr
  contIr <- compile cont
  pure $ HandleIr ir contIr
compile (DefExpr name expr) = do
  void $ modify $ insertDef name
  ir <- compile expr
  pure $ DefIr name ir
compile (InfixExpr assoc name prec expr) = do
  void $ modify $ insertOp assoc name prec expr
  compile expr
compile (ClassExpr name args) = do
  void $ modify $ insertDef name
  pure $ ClassIr name args
compile (ExternExpr name) = do
  void $ modify $ insertDef name
  pure $ IdentIr name

applyOps :: forall m. (MonadError String m) => List (Tuple Op Expr) -> List Op -> List Expr -> m Expr
applyOps Nil Nil (expr : Nil) = pure expr
applyOps Nil ((Op assoc proc expr) : ops) (right : left : exprs) = applyOps Nil ops $ (ApplyExpr expr (left : right : Nil)) : exprs
applyOps ((Tuple op expr) : rest) ops exprs = do 
  (Tuple ops' exprs') <- runOp op ops exprs
  applyOps rest ops' $ expr : exprs'
applyOps _ _ _ = throwError "Invalid operator state" 

runOp :: forall m. (MonadError String m) => Op -> List Op -> List Expr -> m (Tuple (List Op) (List Expr))
runOp op Nil exprs = pure $ Tuple (op : Nil) exprs
runOp op@(Op assoc opPrec expr) (pop@(Op _ popPrec _) : ops) (right : left : exprs) =  
  if popPrec > opPrec || (popPrec == opPrec && assoc == LeftAssoc)
  then runOp op ops $ (ApplyExpr expr (left : right : Nil)) : exprs
  else pure $ Tuple (op : pop : ops) (left : right : exprs)
runOp _ _ _ = throwError "Invalid operator state" 
