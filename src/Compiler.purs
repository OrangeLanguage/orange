module Compiler where

import Prelude

import Control.Monad.State (State, get, put)
import Data.List (List(..), (:))
import Types (Expr(..), Ir(..))

compileCont :: Partial => Expr -> (Ir -> State Int Ir) -> State Int Ir
compileCont (IdentExpr name) f = f $ IdentIr name
compileCont (IntExpr int) f = f $ IntIr int
compileCont (CharExpr char) f = f $ CharIr char
compileCont (StringExpr string) f = f $ StringIr string
compileCont (ApplyExpr expr args) f = 
  compileCont expr \e -> do
    i <- get
    put $ i + 1
    let name = show i
    cont <- f $ IdentIr name
    compileConts args \a -> do 
      pure $ ApplyIr e a name cont

compileConts :: Partial => List Expr -> (List Ir -> State Int Ir) -> State Int Ir
compileConts Nil f = f Nil
compileConts (car : cdr) f = compileCont car \carC -> compileConts cdr \c -> f (carC : c)
