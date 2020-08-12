module Repl where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Bifunctor (lmap)
import Data.Char.Unicode (isSpace)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (drop, length)
import Data.String.CodeUnits (takeWhile, uncons)
import Data.Tuple (Tuple(..))
import Parse (incremental, parseRepl)
import Text.Parsing.Parser (ParseError, ParserT, hoistParserT, runParserT)
import Types (Expr)

foreign import orange :: String -> String

type ReplInput m s = m s

data ReplCommand = Compile Expr | Print Expr

data ReplError e = Parse ParseError | Generic String | Native e

instance showReplError :: Show e => Show (ReplError e) where
  show (Parse e) = show e
  show (Generic s) = s
  show (Native er) = show er

class MonadError e m <= Repl e m | m -> e where
  error :: ReplError e -> m Unit
  query :: String -> m String
  run :: ReplCommand -> m Unit

more :: forall m e. Repl e m => m String
more = query "       > " <#> append "\n"

input :: forall m e. Repl e m => m String
input = query (orange "orange" <> " > ")

parse :: forall m e. Repl e m => String -> m (Maybe Expr)
parse line = do
  let parser = incremental more $ (hoistParserT (unwrap >>> pure) parseRepl :: ParserT String m (Maybe Expr))
  result <- runParserT line parser
  handle Nothing $ lmap Parse result

runCommand :: forall e m. Repl e m => ReplCommand -> m Unit
runCommand command = catchError (run command) (\e -> error $ Native e)

process :: forall m e. Repl e m => String -> m Unit
process line = case uncons line of
  Just ({ head: ':', tail: tail }) -> do
    let (Tuple command expr) = break tail
    case command of
      "compile" -> do
        tree <- parse expr
        maybe (pure unit) (Compile >>> runCommand) tree
      "parse" -> do
        tree <- parse expr
        maybe (pure unit) (Print >>> runCommand) tree
      _ -> error $ Generic $ "Unknown command " <> command
  _ -> do
    tree <- parse line
    maybe (pure unit) (Print >>> runCommand) tree

handle :: forall m a e. Repl e m => a -> Either (ReplError e) a -> m a
handle default result = either (\err -> error err *> pure default) pure result

repl :: forall m e. Repl e m => m Unit
repl = do
  userInput <- input
  process userInput
  repl

break :: String -> Tuple String String
break s = 
  let prefix = takeWhile (not isSpace) s
      postfix = drop (length prefix) s
  in  Tuple prefix postfix