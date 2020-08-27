module Eval where

import Effect (Effect)

foreign import evalString :: String -> Effect String