module Main where

import Prelude

import Compiler (compileCont)
import Control.Monad.State (evalState)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parse (runParseRepl)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = unsafePartial $ case runParseRepl "a(b(), c())" of
    Left error -> logShow error
    Right m -> case m of
      Nothing -> logShow "Nothing"
      Just expr -> logShow $ evalState (compileCont expr pure) 0