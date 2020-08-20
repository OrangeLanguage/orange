module Orange.Tests where

import Prelude

import Data.Int (round)
import Global (readInt)
import Orange.Golden as Golden

incrementTest :: Golden.Test
incrementTest = Golden.basic "Incrementing Works" "test/golden/increment.txt" \input -> do
  let value = round $ readInt 10 input
  pure $ show $ value + 1