module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Parse (runParseRepl)

main :: Effect Unit
main = log $ show $ runParseRepl "\"Hello, world!\""
