module Orange.Golden where

import Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.Path (FilePath)
import Prettier.Printer (colorize)

type Diff = Array { added :: Boolean, removed :: Boolean, value :: String }

foreign import diffImpl :: String -> String -> Diff

newtype Golden = Golden { runTest :: Effect Unit  }

goldenTestPath :: FilePath -> FilePath
goldenTestPath path = path <> ".golden"

mkTest :: Effect Unit -> Golden
mkTest runTest = Golden { runTest }

run :: Golden -> Effect Unit
run (Golden { runTest }) = runTest

runSuite :: Array Golden -> Effect Unit
runSuite tests = do
  traverse_ run tests

defaultResultHandler :: Diff -> Effect Unit
defaultResultHandler diffs = do
    if Array.length diffs == 0
    then do
      log $ colorize "green" "PASSED"
    else do
      log $ colorize "red" "FAILED"

fileTest :: String -> FilePath -> (String -> Effect String) -> (Diff -> Effect Unit) -> Golden
fileTest name file test diffHandler = mkTest do
  log $ name <> " <-" <> (colorize "yellow" $ "'" <> file <> "'")
  fileContents <- readTextFile UTF8 file
  testOutput <- test fileContents
  let goldenFile = goldenTestPath file
  goldenExists <- exists goldenFile
  if goldenExists
  then do
    expectedOutput <- readTextFile UTF8 goldenFile
    diffHandler $ diffImpl testOutput expectedOutput
  else do
    log $ "Creating new .golden file " <> (colorize "yellow" $ "'" <> goldenFile <> "'")
    writeTextFile UTF8 goldenFile testOutput


basic :: String -> FilePath -> (String -> Effect String) -> Golden
basic name file handler = fileTest name file handler defaultResultHandler