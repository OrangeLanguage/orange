module Repl.Node where

import Prelude

import Control.Monad.Cont (ContT(..), lift, runContT)
import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, modify, put)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error) as Js
import Effect.Exception (message, throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (stdout)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Repl (class Repl, ReplCommand(..), ReplError(..))
import Repl as Repl

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
    pure unit
  run (Print tree) = liftEffect $ logShow tree

evalNodeRepl :: forall a. NodeRepl a -> Effect Unit
evalNodeRepl (NodeRepl n) = do
  interface <- createConsoleInterface noCompletion
  runContT (runExceptT (runReaderT n interface)) $ either throwException (const $ pure unit)