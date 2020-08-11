module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Parse (runParseRepl)

main :: Effect Unit
main = log $ show $ runParseRepl "def a = ((infix left + 1 = plus) : a : 1 : 'a' : \"1\" : f(a + 1, a() + 1()))"
