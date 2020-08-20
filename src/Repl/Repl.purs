module Repl where

import Prelude

import Compiler (Compiler, CompilerT, Env(..), transformCompilerT, runCompilerT)
import Compiler as Compiler
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, lift, runReaderT)
import Data.Bifunctor (lmap)
import Data.Char.Unicode (isSpace)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (drop, length)
import Data.String.CodeUnits (takeWhile, uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error) as Js
import Effect.Exception (catchException, error, message, throw)
import Generator as Generator
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Parse (incremental, parseRepl)
import Prettier.Printer (colorize)
import Pretty (showExpr, showIr)
import Text.Parsing.Parser (ParseError, ParserT, hoistParserT, runParserT)
import Types (Expr)

data ReplError e = Parse ParseError | Generic String | Native e

instance showReplError :: Show e => Show (ReplError e) where
  show (Parse e) = show e
  show (Generic s) = s
  show (Native er) = show er

newtype NodeRepl a = NodeRepl (ReaderT Interface (ExceptT Js.Error (CompilerT (ContT Unit Effect))) a)

derive instance newtypeNodeRepl :: Newtype (NodeRepl a) _
derive newtype instance functorNodeRepl :: Functor NodeRepl
derive newtype instance applyNodeRepl :: Apply NodeRepl
derive newtype instance bindNodeRepl :: Bind NodeRepl
derive newtype instance applicativeNodeRepl :: Applicative NodeRepl
derive newtype instance monadNodeRepl :: Monad NodeRepl
derive newtype instance monadEffectNodeRepl :: MonadEffect NodeRepl
derive newtype instance monadAskNodeRepl :: MonadAsk Interface NodeRepl
derive newtype instance monadReaderNodeRepl :: MonadReader Interface NodeRepl
derive newtype instance monadErrorNodeRepl :: MonadError Js.Error NodeRepl
derive newtype instance monadThrowNodeRepl :: MonadThrow Js.Error NodeRepl

tryCompiler :: forall a. Compiler a -> NodeRepl a
tryCompiler ca = do
  result <- liftCompiler $ catchError (ca <#> pure) (\e -> pure $ throwError $ error e)
  result

liftCompiler :: forall a. Compiler a -> NodeRepl a
liftCompiler comp = NodeRepl $ lift $ lift $ transformCompilerT (unwrap >>> pure) comp

evalNodeRepl :: forall a. NodeRepl a -> Effect Unit
evalNodeRepl (NodeRepl n) = do
  interface <- createConsoleInterface noCompletion
  void $ runContT (runCompilerT (runExceptT $ runReaderT n interface) (Env 0 mempty mempty)) $ either throw (const $ pure unit)

handleError :: ReplError Js.Error -> NodeRepl Unit
handleError (Native err) = log $ message err
handleError err = logShow err

query :: String -> NodeRepl String
query prompt = do
  iface <- ask
  NodeRepl $ lift $ lift $ lift $ ContT \cont -> question prompt cont iface

print :: Expr -> NodeRepl Unit
print tree = log $ showExpr 40 tree

more :: NodeRepl String
more = query "       > " <#> append "\n"

input :: NodeRepl String
input = query $ colorize "orange" "orange" <> " > "

parse :: String -> NodeRepl (Maybe Expr)
parse line = do
  let parser = incremental more $ (hoistParserT (unwrap >>> pure) parseRepl :: ParserT String NodeRepl (Maybe Expr))
  result <- runParserT line parser
  handle Nothing $ lmap Parse result
    where 
      handle :: forall a. a -> Either (ReplError Js.Error) a -> NodeRepl a
      handle default result = either (\err -> handleError err *> pure default) pure result

compile :: Expr -> NodeRepl Unit
compile tree = do
  ir <- tryCompiler $ Compiler.compile tree
  log $ showIr 40 ir

generate :: Expr -> NodeRepl Unit
generate expr = do
  ir <- tryCompiler $ Compiler.compile expr
  log $ Generator.generate 0 ir

process :: String -> NodeRepl Unit
process line = case uncons line of
  Just ({ head: ':', tail: tail }) -> do
    let (Tuple command expr) = break tail
    case command of
      "compile" -> do
        tree <- parse expr
        maybe (pure unit) compile tree
      "parse" -> do
        tree <- parse expr
        maybe (pure unit) print tree
      "generate" -> do
        tree <- parse expr
        maybe (pure unit) generate tree
      "load" -> do
        loadFile expr
      "eval" -> do
        log "eval not implemented"
      _ -> handleError $ Generic $ "Unknown command " <> command
  _ -> do
    tree <- parse line
    maybe (pure unit) compile tree

repl ::  NodeRepl Unit
repl = do
  userInput <- input
  catchError (process userInput) logShow
  repl

break :: String -> Tuple String String
break s = 
  let prefix = takeWhile (not isSpace) s
      postfix = drop (length prefix) s
  in  Tuple prefix postfix

loadFile :: FilePath -> NodeRepl Unit
loadFile path = do
  readResult <- liftEffect $ try $ readTextFile UTF8 path
  chars <- either throwError pure readResult
  expr <- parse chars
  maybe (pure unit) (void <<< tryCompiler <<< Compiler.compile) expr
