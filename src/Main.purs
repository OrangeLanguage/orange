module Main where

import Prelude

import Compiler (Env(..), compileCont)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (empty)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parse (runParseRepl)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = unsafePartial $ case runParseRepl "def a = a(a(), a())" of
    Left error -> logShow error
    Right m -> case m of
      Nothing -> logShow "Nothing"
      Just expr -> logShow $ runExceptT $ evalStateT (runReaderT (compileCont expr pure) (Env empty)) 0