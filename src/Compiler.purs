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
  pure $ "_" <> show i

compile :: forall m. Monad m => Expr -> CompilerT m Ir
compile expr = compileCont expr pure 

compileCont :: forall m. Monad m => Expr -> (Ir -> CompilerT m Ir) -> CompilerT m Ir
compileCont (IntExpr int) f = f $ IntIr int
compileCont (CharExpr char) f = f $ CharIr char
compileCont (StringExpr string) f = f $ StringIr string
compileCont (IdentExpr name) f = do
  env <- get
  case lookupDef name env of
    Nothing -> throwError $ "Undefined " <> name
    Just unit -> f $ IdentIr name
compileCont (DotExpr expr name) f = compileCont expr \ir -> f $ DotIr ir name
compileCont (ApplyExpr fun args) f = compileCont fun \funIr -> compileConts args \argsIr -> f $ ApplyIr funIr argsIr
compileCont (OpExpr expr operators) f = do
  env <- get
  ops <- traverse (\(Tuple name e) -> case lookupOp name env of
    Nothing -> throwError $ "Undefined " <> name
    Just op -> pure $ Tuple op e) operators
  apply <- applyOps ops Nil (expr : Nil)
  compileCont apply f
compileCont (BlockExpr exprs) f = compileConts exprs \irs -> f $ BlockIr irs
compileCont (LambdaExpr args expr) f = do
  env <- get
  put $ foldr insertDef env args
  exprIr <- compile expr
  put env
  f $ LambdaIr args exprIr
compileCont (DoExpr expr) f = do
  name <- fresh
  cont <- f $ IdentIr name
  compileCont expr (\ir -> pure $ DoIr ir name cont)
compileCont (HandleExpr expr cont) f = do
  ir <- compile expr
  env <- get
  void $ modify $ insertDef "resume"
  contIr <- compile cont
  f $ HandleIr ir contIr
compileCont (DefExpr Nothing name expr) f = do
  void $ modify $ insertDef name
  ir <- compile expr
  f $ DefIr name ir
compileCont (DefExpr (Just clazz) name expr) f = do
  env <- get
  case lookupDef clazz env of
    Nothing -> throwError $ "Undefined " <> clazz
    Just unit -> do 
      ir <- compile expr
      f $ ExtendIr clazz name ir
compileCont (InfixExpr assoc name prec expr) f = do
  void $ modify $ insertOp assoc name prec expr
  compileCont expr f
compileCont (ClassExpr name args) f = do
  void $ modify $ insertDef name
  f $ ClassIr name args
compileCont (ExternExpr name) f = do
  void $ modify $ insertDef name
  f $ IdentIr name

compileConts :: forall m. Monad m => List Expr -> (List Ir -> CompilerT m Ir) -> CompilerT m Ir
compileConts Nil f = f Nil
compileConts (car : cdr) f = compileCont car \carC -> compileConts cdr \cdrC -> f (carC : cdrC)

applyOps :: forall m. (MonadError String m) => List (Tuple Op Expr) -> List Op -> List Expr -> m Expr
applyOps Nil Nil (expr : Nil) = pure expr
applyOps Nil ((Op assoc proc expr) : ops) (right : left : exprs) = applyOps Nil ops $ (ApplyExpr expr (left : right : Nil)) : exprs
applyOps ((Tuple op expr) : rest) ops exprs = do 
  (Tuple ops' exprs') <- runOp op ops exprs
  applyOps rest ops' $ expr : exprs'
applyOps _ _ _ = throwError "Invalid operator state" 

runOp :: forall m. (MonadError String m) => Op -> List Op -> List Expr -> m (Tuple (List Op) (List Expr))
runOp op Nil exprs = pure $ Tuple (op : Nil) exprs
runOp op@(Op assoc opPrec _) (pop@(Op _ popPrec expr) : ops) (right : left : exprs) =  
  if popPrec > opPrec || (popPrec == opPrec && assoc == LeftAssoc)
  then runOp op ops $ (ApplyExpr expr (left : right : Nil)) : exprs
  else pure $ Tuple (op : pop : ops) (right : left : exprs)
runOp _ _ _ = throwError "Invalid operator state" 
