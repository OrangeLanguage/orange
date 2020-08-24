module Eval where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (Foreign)
import Types (Ir)

foreign import evalString :: String -> Effect String