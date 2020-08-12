module Main where

import Prelude


import Data.Array as Array
import Effect (Effect)
import Node.Process (argv)
import Repl (repl) as Repl
import Repl.Node (evalNodeRepl) as NodeRepl

main :: Effect Unit
main = do
  args <- argv <#> Array.drop 1
  NodeRepl.evalNodeRepl Repl.repl


