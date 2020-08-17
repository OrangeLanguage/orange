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
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect.Class (class MonadEffect)
import Partial.Unsafe (unsafePartial)
import Pretty (showType)
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
unify typ (VarType _) = pure typ
unify (VarType _) typ = pure typ
unify (IdentType n1) (IdentType n2) = if n1 == n2 
  then pure $ IdentType n1
  else throwError $ "Could not unify " <> n1 <> " and " <> n2
unify (FuncType r1 args1) (FuncType r2 args2) = do
  return <- unify r1 r2
  args <- unifyList args1 args2
  pure $ FuncType return args
unify t1 t2 = throwError $ "Could not unify " <> showType 40 t1 <> " and " <> showType 40 t2

unifyMaybe :: forall m. Monad m => Type -> Maybe Type -> CompilerT m Type
unifyMaybe typ Nothing = pure typ
unifyMaybe typ (Just typ') = unify typ typ'

compile :: forall m. Monad m => Expr -> Maybe Type -> CompilerT m (Tuple Ir Type)
compile (IdentExpr name) expected = do
  env <- get
  case lookupGlobal name env of
    Nothing -> throwError $ "Undefined " <> name
    Just typ -> do
      unifiedTyp <- unifyMaybe typ expected
      void $ modify $ insertGlobal name unifiedTyp
      pure $ Tuple (IdentIr name) unifiedTyp
compile (IntExpr int) expected = do
  typ <- unifyMaybe (IdentType "int") expected
  pure $ Tuple (IntIr int) typ
compile (CharExpr char) expected = do
  typ <- unifyMaybe (IdentType "char") expected
  pure $ Tuple (CharIr char) typ
compile (StringExpr string) expected = do
  typ <- unifyMaybe (IdentType "string") expected
  pure $ Tuple (StringIr string) typ
compile (ApplyExpr expr args) expected = do
  argsC <- traverse (\e -> compile e Nothing) args
  i <- fresh
  (Tuple ir typ) <- compile expr (Just $ FuncType (VarType i) $ map snd argsC)
  (Tuple return ts) <- unifyFunction args typ
  pure $ Tuple (ApplyIr ir (map fst argsC)) return
compile (OpExpr expr operators) expected = do
  env <- get
  ops <- traverse (lookup env) operators
  apply <- applyOps ops Nil (expr : Nil)
  compile apply expected
    where
      lookup env (Tuple name e) = case lookupOp name env of
        Nothing -> throwError $ "Undefined " <> name
        Just op -> pure $ Tuple op e
compile (BlockExpr exprs) expected = do
  x <- traverse (\e -> compile e Nothing) exprs
  case last x of
    Nothing -> throwError $ "Empty block"
    Just (Tuple expr typ) -> pure $ Tuple expr typ
compile (LambdaExpr args expr) expected = do
  argTyps <- traverse (\(Tuple name maybeTyp) -> case maybeTyp of 
    Nothing -> Tuple name <$> VarType <$> fresh
    Just typ -> pure $ Tuple name typ) args
  name <- fresh
  (Tuple returnTyp unifiedArgTyps) <- unsafePartial $ do 
    (FuncType r as) <- unifyMaybe (FuncType (VarType name) (map snd argTyps)) expected
    pure $ Tuple r as
  env <- get
  put $ insertGlobals (zip (map fst argTyps) unifiedArgTyps) env
  (Tuple ir typ) <- compile expr $ Just returnTyp
  types <- traverse (\(Tuple arg t) -> snd <$> compile (IdentExpr arg) t) args
  put env
  pure $ Tuple (LambdaIr (map fst args) ir) $ FuncType typ types
compile (DoExpr expr) expected = do
  name <- (<>) "_" <$> fresh
  (Tuple cont contType) <- pure $ Tuple (IdentIr name) (VarType name)
  (Tuple ir typ) <- compile expr Nothing 
  pure $ Tuple (DoIr ir) contType
compile (HandleExpr expr cont) expected = do
  (Tuple ir irTyp) <- compile expr expected
  (Tuple contIr contTyp) <- compile cont expected
  pure $ Tuple (HandleIr ir contIr) contTyp
compile (DefExpr name maybeTyp expr) expected = do
  unifiedTyp <- case maybeTyp of
    Nothing -> VarType <$> fresh
    Just typ -> unifyMaybe typ expected
  void $ modify $ insertGlobal name unifiedTyp
  (Tuple ir irTyp) <- compile expr $ Just unifiedTyp
  pure $ Tuple (DefIr name ir) irTyp
compile (InfixExpr assoc name prec expr) expected = do
  void $ modify $ insertOp assoc name prec expr
  compile expr expected
compile (ExternExpr name typ) expected = do
  unifiedTyp <- unifyMaybe typ expected
  void $ modify $ insertGlobal name typ
  pure $ Tuple (IdentIr name) unifiedTyp

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
