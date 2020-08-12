module Main where

import Prelude

import Compiler (Env(..), compileCont)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Process (argv)
import Parse (runParseRepl)
import Partial.Unsafe (unsafePartial)
import Repl (repl) as Repl
import Repl.Node (evalNodeRepl) as NodeRepl

repl = do
  args <- argv <#> Array.drop 1
  NodeRepl.evalNodeRepl Repl.repl

main :: Effect Unit
main = repl

-- main :: Effect Unit
-- main = unsafePartial $ case runParseRepl "def a = def b = def c = def add = def f = f(infix left + 1 = add, a + b + c)" of
--     Left error -> logShow error
--     Right m -> case m of
--       Nothing -> logShow "Nothing"
--       Just expr -> logShow $ runExceptT $ evalStateT (runReaderT (compileCont expr pure) (Env Set.empty Map.empty)) 0