module Main where

import Prelude
import Data.Array as Array
import Effect (Effect)
import Node.Process (argv)
import Repl (repl, evalNodeRepl) as Repl

main :: Effect Unit
main = do
  args <- argv <#> Array.drop 1
  Repl.evalNodeRepl Repl.repl