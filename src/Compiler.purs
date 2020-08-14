module Compiler where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, put)
import Data.BigInt (BigInt)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Identity (Identity)
import Data.List (List(..), last, (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect.Class (class MonadEffect)
import Types (Assoc(..), Expr(..), Ir(..), Op(..), Type)

newtype CompilerT m a = CompilerT (ReaderT Env (StateT Int (ExceptT String m)) a)
type Compiler a = CompilerT Identity a

derive instance newtypeCompiler :: Newtype (CompilerT m a) _
derive newtype instance monadErrorCompiler :: Monad m => MonadError String (CompilerT m)
derive newtype instance functorCompiler :: Functor m => Functor (CompilerT m)
derive newtype instance applyCompiler :: Monad m => Apply (CompilerT m)
derive newtype instance applicativeCompiler :: Monad m => Applicative (CompilerT m)
derive newtype instance monadCompiler :: Monad m => Monad (CompilerT m) 
derive newtype instance monadStateCompiler :: Monad m => MonadState Int (CompilerT m)
derive newtype instance monadEffectCompiler :: MonadEffect m => MonadEffect (CompilerT m)
derive newtype instance monadThrowCompiler :: Monad m => MonadThrow String (CompilerT m)
derive newtype instance bindCompiler :: Monad m => Bind (CompilerT m)
derive newtype instance monadAskCompiler :: Monad m => MonadAsk Env (CompilerT m)
derive newtype instance monadReaderCompiler :: Monad m => MonadReader Env (CompilerT m)

runCompilerT :: forall m a. Monad m => CompilerT m a -> m (Either String a)
runCompilerT compiler = runExceptT $ evalStateT (runReaderT (unwrap compiler) (Env empty empty)) 0

runCompiler :: forall a. Compiler a -> Either String a
runCompiler compiler = unwrap $ runCompilerT compiler

data Env = Env (Map String (Maybe Type)) (Map String Op)

insertGlobal :: String -> Maybe Type -> Env -> Env
insertGlobal name typ (Env globals ops) = Env (insert name typ globals) ops

insertGlobals :: List (Tuple String (Maybe Type)) -> Env -> Env
insertGlobals names env = foldr (uncurry insertGlobal) env names

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env globals ops) = Env globals (insert name (Op assoc prec expr) ops)

lookupGlobal :: String -> Env -> Maybe (Maybe Type)
lookupGlobal name (Env globals ops) = lookup name globals

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env globals ops) = lookup name ops

fresh :: forall m. Monad m => CompilerT m String
fresh = do
  i <- get
  put $ i + 1
  pure $ "_" <> show i

compile :: forall m. Monad m => Expr -> CompilerT m Ir
compile expr = compileCont expr pure

compileCont :: forall m. Monad m => Expr -> (Ir -> CompilerT m Ir) -> CompilerT m Ir
compileCont (IdentExpr name) f = do
  env <- ask
  case lookupGlobal name env of
    Nothing -> throwError $ "Undefined " <> name
    Just typ -> f $ IdentIr name
compileCont (IntExpr int) f = f $ IntIr int
compileCont (CharExpr char) f = f $ CharIr char
compileCont (StringExpr string) f = f $ StringIr string
compileCont (ApplyExpr expr args) f = compileCont expr \e -> compileConts args \a -> f $ ApplyIr e a 
compileCont (OpExpr expr operators) f = do
  env <- ask
  ops <- traverse (lookup env) operators
  apply <- applyOps ops Nil (expr : Nil)
  compileCont apply f
    where
      lookup env (Tuple name e) = case lookupOp name env of
        Nothing -> throwError $ "Undefined " <> name
        Just op -> pure $ Tuple op e
compileCont (BlockExpr exprs) f = compileConts exprs \x -> case last x of
  Nothing -> throwError $ "Empty block"
  Just l -> f l
compileCont (LambdaExpr names expr) f = do
  ir <- local (insertGlobals names) $ compile expr
  f $ LambdaIr (map fst names) ir
compileCont (DoExpr expr) f = do
  name <- fresh
  cont <- f $ IdentIr name
  compileCont expr (\ir -> pure $ DoIr ir name cont)
compileCont (HandleExpr expr cont) f = do
  ir <- compile expr
  local (insertGlobal "resume" Nothing) $ compileCont cont \contIr -> pure $ HandleIr ir contIr
compileCont (DefExpr name typ expr) f = do 
  ir <- local (insertGlobal name typ) $ compile expr
  DefIr name ir <$> (local (insertGlobal name typ) $ f (IdentIr name))
compileCont (InfixExpr assoc name prec expr) f = local (insertOp assoc name prec expr) $ compileCont expr f
compileCont (ExternExpr name typ) f = local (insertGlobal name (Just typ)) (f $ IdentIr name)

compileConts :: forall m. Monad m => List Expr -> (List Ir -> CompilerT m Ir) -> CompilerT m Ir
compileConts Nil f = f Nil
compileConts (car : cdr) f = compileCont car \carC -> compileConts cdr \c -> f (carC : c)

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
  if popPrec > opPrec || (popPrec == opPrec && assoc == Left)
  then runOp op ops ((ApplyExpr expr (left : right : Nil)) : exprs)
  else pure $ Tuple (op : pop : ops) (left : right : exprs)
runOp _ _ _ = throwError "Invalid operator state" 
