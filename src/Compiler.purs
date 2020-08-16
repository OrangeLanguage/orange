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
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect.Class (class MonadEffect)
import Pretty (showType)
import Types (Assoc(..), Expr(..), Ir(..), Op(..), Type(..))

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

data Env = Env (Map String Type) (Map String Op)

insertGlobal :: String -> Type -> Env -> Env
insertGlobal name typ (Env globals ops) = Env (insert name typ globals) ops

insertGlobals :: List (Tuple String Type) -> Env -> Env
insertGlobals names env = foldr (uncurry insertGlobal) env names

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env globals ops) = Env globals (insert name (Op assoc prec expr) ops)

lookupGlobal :: String -> Env -> Maybe Type
lookupGlobal name (Env globals ops) = lookup name globals

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env globals ops) = lookup name ops

fresh :: forall m. Monad m => CompilerT m String
fresh = do
  i <- get
  put $ i + 1
  pure $ "_" <> show i

compile :: forall m. Monad m => Expr -> Type -> CompilerT m (Tuple Ir Type)
compile expr expected = compileCont expr expected (\ir typ -> pure $ Tuple ir typ)

unifyFunction :: forall m. Monad m => List Expr -> Type -> CompilerT m (Tuple Type (List (Tuple Expr Type)))
unifyFunction exprs (FuncType return args) = do
  ts <- unifyTypes exprs args
  pure $ Tuple return ts
unifyFunction exprs typ = throwError "Not a function type"

unifyTypes :: forall m. Monad m => List Expr -> List Type -> CompilerT m (List (Tuple Expr Type))
unifyTypes Nil Nil = pure Nil
unifyTypes (expr : exprs) (typ : typs) = do
  rest <- unifyTypes exprs typs
  pure $ (Tuple expr typ) : rest
unifyTypes exprs typs = throwError "Could not unify"

unifyList :: forall m. Monad m => List Type -> List Type -> CompilerT m (List Type)
unifyList Nil Nil = pure Nil
unifyList (car1 : cdr1) (car2 : cdr2) = do
  car <- unify car1 car2
  cdr <- unifyList cdr1 cdr2
  pure $ car : cdr
unifyList l1 l2 = throwError "Mismatched lists"

unify :: forall m. Monad m => Type -> Type -> CompilerT m Type
unify typ AnyType = pure typ
unify AnyType typ = pure typ
unify (IdentType n1) (IdentType n2) = if n1 == n2 
  then pure $ IdentType n1
  else throwError $ "Could not unify " <> n1 <> " and " <> n2
unify (FuncType r1 args1) (FuncType r2 args2) = do
  return <- unify r1 r2
  args <- unifyList args1 args2
  pure $ FuncType return args
unify t1 t2 = throwError $ "Could not unify " <> showType 40 t1 <> " and " <> showType 40 t2

compileCont :: forall m. Monad m => Expr -> Type -> (Ir -> Type -> CompilerT m (Tuple Ir Type)) -> CompilerT m (Tuple Ir Type)
compileCont (IdentExpr name) expected f = do
  env <- ask
  case lookupGlobal name env of
    Nothing -> throwError $ "Undefined " <> name
    Just typ -> do
      unifiedTyp <- unify typ expected
      local (insertGlobal name unifiedTyp) $ f (IdentIr name) unifiedTyp
compileCont (IntExpr int) expected f = do
  typ <- unify (IdentType "int") expected
  f (IntIr int) typ
compileCont (CharExpr char) expected f = do
  typ <- unify (IdentType "char") expected
  f (CharIr char) typ
compileCont (StringExpr string) expected f = do
  typ <- unify (IdentType "string") expected
  f (StringIr string) typ
compileCont (ApplyExpr expr args) expected f = 
  compileConts (map (\e -> Tuple e AnyType) args) \irs -> 
    compileCont expr (FuncType expected $ map snd irs) \ir typ -> do 
      (Tuple return ts) <- unifyFunction args typ
      compileConts ts \irs' -> f (ApplyIr ir (map fst irs')) return
compileCont (OpExpr expr operators) expected f = do
  env <- ask
  ops <- traverse (lookup env) operators
  apply <- applyOps ops Nil (expr : Nil)
  compileCont apply expected f
    where
      lookup env (Tuple name e) = case lookupOp name env of
        Nothing -> throwError $ "Undefined " <> name
        Just op -> pure $ Tuple op e
compileCont (BlockExpr exprs) expected f = compileConts (map (\expr -> Tuple expr AnyType) exprs) \x -> case last x of
  Nothing -> throwError $ "Empty block"
  Just (Tuple expr typ) -> f expr typ
compileCont (LambdaExpr args expr) expected f = do
  (Tuple ir typ) <- local (insertGlobals args) $ compileCont expr AnyType \ir typ -> do
    types <- traverse (\(Tuple arg t) -> snd <$> compile (IdentExpr arg) t) args
    pure $ Tuple ir (FuncType typ types)
  f (LambdaIr (map fst args) ir) typ
compileCont (DoExpr expr) expected f = do
  name <- fresh
  (Tuple cont contType) <- f (IdentIr name) expected
  compileCont expr AnyType (\ir typ -> pure $ Tuple (DoIr ir name cont) contType)
compileCont (HandleExpr expr cont) expected f = do
  (Tuple ir irTyp) <- compile expr expected
  (Tuple contIr contTyp) <- compile cont expected
  f (HandleIr ir contIr) contTyp
compileCont (DefExpr name typ expr) expected f = do
  unifiedTyp <- unify expected typ
  (Tuple ir irTyp) <- local (insertGlobal name unifiedTyp) $ compile expr unifiedTyp
  unifiedIrTyp <- unify unifiedTyp irTyp
  (Tuple cont contTyp) <- local (insertGlobal name unifiedIrTyp) $ f (IdentIr name) unifiedIrTyp
  pure $ Tuple (DefIr name ir cont) contTyp
compileCont (InfixExpr assoc name prec expr) expected f = local (insertOp assoc name prec expr) $ compileCont expr expected f
compileCont (ExternExpr name typ) expected f = do
  unifiedTyp <- unify expected typ 
  local (insertGlobal name typ) $ f (IdentIr name) unifiedTyp

compileConts :: forall m. Monad m => List (Tuple Expr Type) -> (List (Tuple Ir Type) -> CompilerT m (Tuple Ir Type)) -> CompilerT m (Tuple Ir Type)
compileConts Nil f = f Nil
compileConts ((Tuple expr typ) : cdr) f = 
  compileCont expr typ \carIr carTyp -> 
    compileConts cdr \irs -> f $ (Tuple carIr carTyp) : irs

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
