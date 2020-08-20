module Repl where

import Prelude

import Compiler (Compiler, CompilerT, Env(..), transformCompilerT, runCompilerT)
import Compiler as Compiler
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, lift, runReaderT)
import Data.Char.Unicode (isSpace)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (drop, length)
import Data.String.CodeUnits (takeWhile, uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error) as Js
import Effect.Exception (message, throw)
import Generator as Generator
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Parse (incremental, parseRepl)
import Prettier.Printer (colorize)
import Pretty (showExpr, showIr)
import Text.Parsing.Parser (ParseError, ParserT, hoistParserT, runParserT)
import Types (Expr, Ir)

data ReplError = Parse ParseError | Generic String | Native Js.Error

instance showReplError :: Show ReplError where
  show (Parse e) = show e
  show (Generic s) = s
  show (Native e) = show e

newtype NodeRepl a = NodeRepl (ReaderT Interface (ExceptT ReplError (CompilerT (ContT Unit Effect))) a)

derive instance newtypeNodeRepl :: Newtype (NodeRepl a) _
derive newtype instance functorNodeRepl :: Functor NodeRepl
derive newtype instance applyNodeRepl :: Apply NodeRepl
derive newtype instance bindNodeRepl :: Bind NodeRepl
derive newtype instance applicativeNodeRepl :: Applicative NodeRepl
derive newtype instance monadNodeRepl :: Monad NodeRepl
derive newtype instance monadEffectNodeRepl :: MonadEffect NodeRepl
derive newtype instance monadAskNodeRepl :: MonadAsk Interface NodeRepl
derive newtype instance monadReaderNodeRepl :: MonadReader Interface NodeRepl
derive newtype instance monadErrorNodeRepl :: MonadError ReplError NodeRepl
derive newtype instance monadThrowNodeRepl :: MonadThrow ReplError NodeRepl

tryCompile :: String -> NodeRepl Ir
tryCompile program = do
  tree <- parse program
  tryCompiler $ Compiler.compile tree

tryCompiler :: forall a. Compiler a -> NodeRepl a
tryCompiler ca = join $ liftCompiler $ catchError (ca <#> pure) (pure <<< throwError <<< Generic)

liftCompiler :: forall a. Compiler a -> NodeRepl a
liftCompiler comp = NodeRepl $ lift $ lift $ transformCompilerT (unwrap >>> pure) comp

evalNodeRepl :: forall a. NodeRepl a -> Effect Unit
evalNodeRepl nodeRepl = do
  interface <- createConsoleInterface noCompletion
  void $ runContT (runCompilerT (runExceptT $ runReaderT (unwrap nodeRepl) interface) (Env 0 mempty mempty)) $ either throw (const $ pure unit)

query :: String -> NodeRepl String
query prompt = do
  iface <- ask
  NodeRepl $ lift $ lift $ lift $ ContT \cont -> question prompt cont iface

print :: String -> NodeRepl Unit
print expr = do
  tree <- parse expr
  log $ showExpr 40 tree

more :: NodeRepl String
more = query "       > " <#> append "\n"

input :: NodeRepl String
input = query $ colorize "orange" "orange" <> " > "

parse :: String -> NodeRepl Expr
parse line = do
  let parser = incremental more $ (hoistParserT (unwrap >>> pure) parseRepl :: ParserT String NodeRepl (Maybe Expr))
  result <- runParserT line parser
  maybeResult <- either (throwError <<< Parse) pure result
  maybe (throwError $ Generic "Unable to parse") pure maybeResult

handleError :: ReplError -> NodeRepl Unit
handleError (Native err) = log $ message err
handleError err = logShow err

compile :: String -> NodeRepl Unit
compile expr = do
  ir <- tryCompile expr
  log $ showIr 40 ir

generate :: String -> NodeRepl Unit
generate expr = do
  ir <- tryCompile expr
  log $ Generator.generate 0 ir

process :: String -> NodeRepl Unit
process line = case uncons line of
  Just ({ head: ':', tail: tail }) -> do
    let (Tuple command expr) = break tail
    case command of
      "compile" -> compile expr
      "parse" -> print expr
      "generate" -> generate expr
      "load" -> loadFile expr
      "eval" -> log "eval not implemented"
      _ -> handleError $ Generic $ "Unknown command " <> command
  _ -> compile line

repl ::  NodeRepl Unit
repl = do
  userInput <- input
  catchError (process userInput) handleError
  repl

break :: String -> Tuple String String
break s = 
  let prefix = takeWhile (not isSpace) s
      postfix = drop (length prefix) s
  in  Tuple prefix postfix

loadFile :: FilePath -> NodeRepl Unit
loadFile path = do
  readResult <- liftEffect $ try $ readTextFile UTF8 path
  chars <- either (throwError <<< Native) pure readResult
  expr <- parse chars
  void $ tryCompiler $ Compiler.compile expr
