module Test.Golden where

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

newtype Test = Test { runTest :: Effect Unit  }

goldenTestPath :: FilePath -> FilePath
goldenTestPath path = path <> ".golden"

mkTest :: Effect Unit -> Test
mkTest runTest = Test { runTest }

run :: Test -> Effect Unit
run (Test { runTest }) = runTest

runSuite :: Array Test -> Effect Unit
runSuite tests = do
  traverse_ run tests

defaultResultHandler :: Diff -> Effect Unit
defaultResultHandler diffs = do
    if Array.length diffs == 0
    then log $ colorize "green" "PASSED"
    else do
      log $ colorize "red" "FAILED"
      traverse_ printDiff diffs
    where
      printDiff diff = log $ if diff.added
        then colorize "green" $ "+ " <> diff.value
        else colorize "red" $ "- "  <> diff.value

fileTest :: String -> FilePath -> (String -> Effect String) -> (Diff -> Effect Unit) -> Test
fileTest name file test diffHandler = mkTest do
  log $ name <> " <- " <> (colorize "yellow" $ "'" <> file <> "'")
  fileContents <- readTextFile UTF8 file
  testOutput <- test fileContents
  let goldenFile = goldenTestPath file
  goldenExists <- exists goldenFile
  if goldenExists
  then do
    expectedOutput <- readTextFile UTF8 goldenFile
    diffHandler $ diffImpl expectedOutput testOutput
  else do
    log $ (colorize "green" "+ ") <> (colorize "yellow" $ "'" <> goldenFile <> "'")
    writeTextFile UTF8 goldenFile testOutput

basic :: String -> FilePath -> (String -> Effect String) -> Test
basic name file inputHandler = fileTest name file inputHandler defaultResultHandler