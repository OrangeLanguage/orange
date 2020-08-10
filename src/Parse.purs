module Parse where

import Prelude

import Control.Alternative (empty, (<|>))
import Data.Array (many, null, some)
import Data.BigInt (fromString)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseError, fail, runParser)
import Text.Parsing.Parser (Parser) as P
import Text.Parsing.Parser.Combinators (option, skipMany, try)
import Text.Parsing.Parser.String (char, eof, noneOf, oneOf, string)
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

parseIdent :: Parser Expr
parseIdent = do
  head <- letter
  tail <- fromCharArray <$> many (letter <|> digit)
  ignored
  pure $ IdentExpr $ singleton head <> tail

parseInteger :: Parser Expr
parseInteger = do
  sign <- option "" $ string "-"
  number <- fromCharArray <$> some digit
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
  chars <- fromCharArray <$> many (try escape <|> noneOf [ '\"' ])
  void $ char '"'
  ignored
  pure $ StringExpr chars

parseParens :: Parser Expr
parseParens = do
  void $ char '('
  ignored
  expr <- parseExpr unit
  void $ char ')'
  ignored
  pure $ expr

parseAtomic :: Unit -> Parser Expr
parseAtomic unit = parseIdent <|> parseInteger <|> parseChar <|> parseString <|> parseParens

parseOperator :: Parser (Tuple String Expr)
parseOperator = do
  name <- fromCharArray <$> many (oneOf $ toCharArray "~`!@$%^&*-=+\\|;:<>,.?")
  ignored
  expr <- parseAtomic unit
  pure $ Tuple name expr

parseOperators :: Unit -> Parser Expr
parseOperators unit = do
  expr <- parseAtomic unit
  operators <- many parseOperator
  pure $ if null operators
    then expr
    else OperatorExpr expr operators

parseExpr :: Unit -> Parser Expr
parseExpr unit = parseOperators unit

parseRepl :: Parser (Maybe Expr)
parseRepl = ignored *> (option Nothing (Just <$> parseExpr unit)) <* ignored <* eof

runParseRepl :: String -> Either ParseError (Maybe Expr)
runParseRepl input = runParser input parseRepl