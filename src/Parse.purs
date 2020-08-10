module Parse where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (many, some)
import Data.BigInt (fromString)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, singleton)
import Text.Parsing.Parser (ParseError, fail, runParser)
import Text.Parsing.Parser (Parser) as P
import Text.Parsing.Parser.Combinators (option, skipMany, try)
import Text.Parsing.Parser.String (char, eof, noneOf, string)
import Text.Parsing.Parser.Token (digit, letter, space)
import Types (Expr(..))

type Parser a = P.Parser String a
data ParsingError = ParsingError String ParseError

ignored :: Parser Unit
ignored = skipMany $ void space

escape :: Parser Char
escape = do
  void $ string "\\\""
  pure '"'

identHead :: Parser Char
identHead = letter

identTail :: Parser String
identTail = many (identHead <|> digit) <#> fromCharArray

parseIdent :: Parser Expr
parseIdent = do
  head <- identHead
  tail <- identTail
  ignored
  pure $ IdentExpr $ singleton head <> tail

parseInteger :: Parser Expr
parseInteger = do
  sign <- option "" $ string "-"
  number <- some digit <#> fromCharArray
  ignored
  result <- fromMaybe (fail "Invalid number") $ map pure $ fromString $ sign <> number
  pure $ IntExpr result

parseChar :: Parser Expr
parseChar = do
  void $ char '\''
  c <- try escape <|> noneOf [ '\'' ]
  void $ char '\''
  ignored
  pure $ CharExpr c

parseString :: Parser Expr
parseString = do
  void $ char '"'
  chars <- many (try escape <|> noneOf [ '\"' ]) <#> fromCharArray
  void $ char '"'
  ignored
  pure $ StringExpr chars

parseExpr :: Unit -> Parser Expr
parseExpr unit = parseIdent <|> parseInteger <|> parseChar <|> parseString

parseRepl :: Parser (Maybe Expr)
parseRepl = (ignored *> (option Nothing $ parseExpr unit <#> Just) <* ignored <* eof)

runParseRepl :: String -> Either ParseError (Maybe Expr)
runParseRepl input = runParser input parseRepl