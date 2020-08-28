module Import where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError, try)
import Data.Either (either)
import Data.List (List)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (message)
import Node.Encoding (Encoding(..)) 
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Parse (parseProgram)
import Text.Parsing.Parser (runParser)
import Types (Expr)

importFile :: forall m. MonadEffect m => MonadError String m => FilePath -> m (List Expr)
importFile path = do
  readResult <- liftEffect $ try $ readTextFile UTF8 path
  chars <- either (throwError <<< message) pure readResult
  either (throwError <<< show) pure $ runParser chars parseProgram