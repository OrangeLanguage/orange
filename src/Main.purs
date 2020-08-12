module Main where

import Prelude

import Compiler (Env(..), compileCont)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Map as Map
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parse (runParseRepl)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = unsafePartial $ case runParseRepl "def a = def b = def c = def add = def f = f(infix left + 1 = add, a + b + c)" of
    Left error -> logShow error
    Right m -> case m of
      Nothing -> logShow "Nothing"
      Just expr -> logShow $ runExceptT $ evalStateT (runReaderT (compileCont expr pure) (Env Set.empty Map.empty)) 0