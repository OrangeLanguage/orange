module Compiler where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (class MonadTrans, ExceptT(..), mapExceptT, runExceptT, throwError)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, execStateT, get, mapStateT, modify, put)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.List (List(..), foldr, intercalate, singleton, (:))
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Generator (escape)
import Import (importFile)
import Types (Arg(..), Assoc(..), Eval(..), Expr(..), Ir(..), Op(..), Pattern(..))

newtype CompilerT m a = CompilerT (StateT Env (ExceptT String m) a)
type Compiler a = CompilerT Effect a

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

runCompiler :: forall a. Compiler a -> Env -> Effect (Either String Env)
runCompiler compiler env = runCompilerT compiler env

evalCompilerT :: forall m a. Monad m => CompilerT m a -> Env -> m (Either String a)
evalCompilerT compiler env = runExceptT $ evalStateT (unwrap compiler) env

evalCompiler :: forall a. Compiler a -> Env -> Effect (Either String a)
evalCompiler compiler env = evalCompilerT compiler env

data Env = Env Int (Map String Op)

insertOp :: Assoc -> String -> BigInt -> Expr -> Env -> Env
insertOp assoc name prec expr (Env i ops) = Env i (insert name (Op assoc prec expr) ops)

lookupOp :: String -> Env -> Maybe Op
lookupOp name (Env i ops) = lookup name ops

fresh :: forall m. Monad m => CompilerT m String
fresh = do
  (Env i ops) <- get
  put $ Env (i + 1) ops
  pure $ "_" <> show i

compile :: forall m. Monad m => MonadEffect m => Expr -> CompilerT m (List Ir)
compile expr = compileCont expr (singleton >>> pure)

compileCont :: forall m. Monad m => MonadEffect m => Expr -> (Ir -> CompilerT m (List Ir)) -> CompilerT m (List Ir)
compileCont (BoolExpr int) f = f $ BoolIr int
compileCont (IntExpr int) f = f $ IntIr int
compileCont (CharExpr char) f = f $ CharIr char
compileCont (StringExpr string) f = f $ StringIr string
compileCont (IdentExpr name) f = f $ IdentIr name
compileCont (DotExpr expr name) f = compileCont expr \ir -> f $ DotIr ir name
compileCont (ApplyExpr fun args) f = 
  compileCont fun \funIr -> 
    compileConts (map (LambdaExpr Nil) args) \argsIr -> do
      name <- fresh
      cont <- f $ IdentIr name 
      pure $ singleton $ ApplyIr funIr (argsIr <> singleton (LambdaIr (singleton name) (BlockIr cont)))
compileCont (OpExpr expr operators) f = do
  env <- get
  ops <- traverse (\(Tuple name e) -> case lookupOp name env of
    Nothing -> throwError $ "Undefined " <> name
    Just op -> pure $ Tuple op e) operators
  apply <- applyOps ops Nil (expr : Nil)
  compileCont apply f
compileCont (BlockExpr (expr : Nil)) f = compileCont expr f
compileCont (BlockExpr exprs) f = compileConts exprs \irs -> f $ BlockIr irs
compileCont (LambdaExpr args expr) f = do
  env <- get
  exprIr <- compileCont expr (\x -> pure $ singleton $ ApplyIr (IdentIr "_cont") (x : Nil))
  let evalIr = foldr evalArg (BlockIr exprIr) args
  put env
  f $ LambdaIr (map (\(Arg _ name) -> name) args <> singleton "_cont") evalIr
compileCont (DoExpr expr) f = do
  name <- fresh
  cont <- f $ IdentIr name
  compileCont expr (\ir -> pure $ singleton $ DoIr ir name (BlockIr cont))
compileCont (HandleExpr expr cont) f = do
  ir <- compile expr
  let resumeExpr = ApplyExpr (LambdaExpr (singleton $ Arg EagerEval "resume") cont) (singleton $ ExternExpr "_e.resume")
  contIr <- compile $ ApplyExpr resumeExpr (singleton $ ExternExpr "_e.effect")
  f $ HandleIr (BlockIr ir) (BlockIr contIr)
compileCont (MatchExpr expr patterns) f = compileCont (desugarMatch expr patterns) f
compileCont (DefExpr Nothing name expr) f = do
  irs <- compile expr
  cont <- f $ IdentIr name
  pure $ DefIr name (BlockIr irs) : cont
compileCont (DefExpr (Just clazz) name expr) f = do
  irs <- compile expr
  cont <- f $ BlockIr Nil
  pure $ ExtendIr clazz name (BlockIr irs) : cont
compileCont (LetExpr name expr) f = do
  irs <- compile (LambdaExpr Nil expr)
  cont <- f $ IdentIr name
  let lambda = LambdaIr (singleton name) (evalArg (Arg EagerEval name) (BlockIr cont))
  pure $ singleton $ ApplyIr lambda (singleton $ BlockIr irs)
compileCont (InfixExpr assoc name prec expr) f = do
  void $ modify $ insertOp assoc name prec expr
  compileCont expr f
compileCont (ClassExpr name args) f = do 
  let argNames = map (\(Arg _ n) -> n) args
  constructor <- compile $ 
    DefExpr Nothing name $ 
      LambdaExpr args $ 
        ExternExpr ("new _" <> name <> "(" <> intercalate ", " (map escape argNames) <> ")")
  matchAny <- compile $ 
    DefExpr (Just "Any") ("as" <> name) $ 
      LambdaExpr (Arg EagerEval "f" : Arg LazyEval "cont" : Nil) $
        ApplyExpr (IdentExpr "cont") Nil
  matchThis <- compile $
    DefExpr (Just name) ("as" <> name) $
      LambdaExpr (Arg EagerEval "f" : Arg LazyEval "cont" : Nil) $
        ApplyExpr (IdentExpr "f") (map (\n -> DotExpr (IdentExpr "this") n) argNames)
  cont <- f $ IdentIr name
  pure $ singleton (ClassIr name argNames) <> matchAny <> matchThis <> constructor <> cont
compileCont (ImportExpr name) f = do
  exprs <- importFile name
  irs <- traverse compile exprs
  last <- f $ IdentIr "_unit"
  pure $ (irs >>= identity) <> last
compileCont (ExternExpr string) f = f $ ExternIr string

compileConts :: forall m. Monad m => MonadEffect m => List Expr -> (List Ir -> CompilerT m (List Ir)) -> CompilerT m (List Ir)
compileConts Nil f = f Nil
compileConts (car : cdr) f = compileCont car \carC -> compileConts cdr \cdrC -> f (carC : cdrC)

evalArg :: Arg -> Ir -> Ir
evalArg (Arg EagerEval name) ir = ApplyIr (IdentIr name) (singleton (LambdaIr (singleton name) ir))
evalArg (Arg LazyEval name) ir = ir

desugarMatch :: Expr -> List Pattern -> Expr
desugarMatch expr Nil = IdentExpr "undefined"
desugarMatch expr ((Pattern name args cas) : patterns) =
  let dot = DotExpr expr ("as" <> name)
      lambda = LambdaExpr args cas
  in ApplyExpr dot (lambda : desugarMatch expr patterns : Nil)

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
