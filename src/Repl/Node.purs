module Repl.Node where

import Prelude

import Compiler (compileCont, runCompiler)
import Control.Monad.Cont (ContT(..), lift, runContT)
import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error) as Js
import Effect.Exception (message, throwException)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Pretty (showExpr, showIr)
import Repl (class Repl, ReplCommand(..), ReplError(..))

foreign import getCharImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

getChar :: Effect (Maybe Char)
getChar = getCharImpl Just Nothing <#> \m -> m >>= String.charAt 0

newtype NodeRepl a = NodeRepl (ReaderT Interface (ExceptT Js.Error (ContT Unit Effect)) a)

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

tryEffect :: forall a. Effect a -> NodeRepl a
tryEffect eff = do
  result <- liftEffect $ try eff
  either throwError pure result

instance replNodeRepl :: Repl Js.Error NodeRepl where
  error (Native err) = liftEffect $ log $ message err
  error err = liftEffect $ logShow err
  query prompt = do
    iface <- ask
    NodeRepl $ lift $ lift $ ContT \cont -> question prompt cont iface
  run (Compile tree) = do
    let result = runCompiler $ compileCont tree pure
    either logShow (log <<< showIr 20) result
  run (Print tree) = log $ showExpr 20 tree

evalNodeRepl :: forall a. NodeRepl a -> Effect Unit
evalNodeRepl (NodeRepl n) = do
  interface <- createConsoleInterface noCompletion
  runContT (runExceptT $ runReaderT n interface) $ either throwException (const $ pure unit)