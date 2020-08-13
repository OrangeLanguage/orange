module Parse where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (get, lift, put)
import Data.Array (foldl)
import Data.Array as Array
import Data.BigInt (BigInt, fromString)
import Data.Either (Either)
import Data.List (List, many, snoc)
import Data.List (null) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT, fail, runParser)
import Text.Parsing.Parser (Parser) as P
import Text.Parsing.Parser.Combinators (option, sepBy, skipMany, try)
import Text.Parsing.Parser.String (class StringLike, char, eof, noneOf, oneOf, string, null)
import Text.Parsing.Parser.Token (digit, letter, space)
import Types (Assoc(..), Expr(..))

type Parser a = P.Parser String a
data ParsingError = ParsingError String ParseError

incremental :: forall m s a. StringLike s => Semigroup s => Monad m => m s -> ParserT s m a -> ParserT s m a
incremental more incrementalParser = do
  (ParseState previous position _) <- get
  catchError (try incrementalParser) \e -> do
    (ParseState leftOver _ consumed) <- get
    if null leftOver
      then do
        extra <- lift $ more
        put $ ParseState (previous <> extra) position consumed
        incremental more incrementalParser
      else
        throwError e

ignored :: Parser Unit
ignored = skipMany space

escape :: Parser Char
escape = do
  void $ string "\\\""
  pure '"'

parseIdent :: Parser String
parseIdent = do
  head <- letter
  tail <- fromCharArray <$> Array.many (letter <|> digit)
  ignored
  pure $ singleton head <> tail

parseInt :: Parser BigInt
parseInt = do
  sign <- option "" $ string "-"
  number <- fromCharArray <$> Array.some digit
  ignored
  result <- fromMaybe (fail "Invalid number") $ map pure $ fromString $ sign <> number
  pure result

parseChar :: Parser Char
parseChar = do
  void $ char '\''
  c <- try escape <|> noneOf [ '\'' ]
  void $ char '\''
  ignored
  pure c

parseString :: Parser String
parseString = do
  void $ char '"'
  chars <- fromCharArray <$> Array.many (try escape <|> noneOf [ '\"' ])
  void $ char '"'
  ignored
  pure chars

parseParens :: Parser Expr
parseParens = do
  void $ char '('
  ignored
  expr <- parseExpr unit
  void $ char ')'
  ignored
  pure expr

parseAtomic :: Unit -> Parser Expr
parseAtomic unit = 
  IdentExpr <$> parseIdent <|> 
  IntExpr <$> parseInt <|> 
  CharExpr <$> parseChar <|> 
  StringExpr <$> parseString <|> 
  parseParens

parseApply :: Parser (List Expr)
parseApply = do 
  void $ char '('
  ignored
  args <- sepBy (parseExpr unit) (char ',' *> ignored)
  void $ char ')'
  ignored
  last <- option Nothing $ Just <$> parseExpr unit
  pure $ case last of
    Nothing -> args
    Just expr -> snoc args expr

parseApplies :: Unit -> Parser Expr
parseApplies unit = do
  expr <- parseAtomic unit
  applies <- many parseApply
  pure $ foldl ApplyExpr expr applies

parseOp :: Parser String
parseOp = do
  name <- fromCharArray <$> Array.some (oneOf $ toCharArray "~`!@$%^&*-=+\\|:<>.?")
  ignored
  if name == "->"
    then fail "Reserved operator ->" 
    else pure name

parseOperator :: Parser (Tuple String Expr)
parseOperator = do
  name <- parseOp
  expr <- parseApplies unit
  pure $ Tuple name expr

parseOperators :: Unit -> Parser Expr
parseOperators unit = do
  expr <- parseApplies unit
  operators <- many parseOperator
  pure $ if List.null operators
    then expr
    else OpExpr expr operators

parseBlock :: Parser Expr
parseBlock = do
  void $ char '{'
  ignored
  exprs <- sepBy (parseExpr unit) (char ';' <* ignored)
  void $ char '}'
  ignored
  pure $ BlockExpr exprs

parseLambda :: Parser Expr
parseLambda = do
  names <- sepBy parseIdent (char ',' <* ignored)
  ignored
  void $ string "->"
  ignored
  expr <- parseExpr unit
  pure $ LambdaExpr names expr

parseDo :: Parser Expr
parseDo = do
  void $ string "do"
  ignored
  expr <- parseExpr unit
  pure $ DoExpr expr

parseHandle :: Parser Expr
parseHandle = do
  void $ string "handle"
  ignored
  expr <- parseAtomic unit
  cont <- parseExpr unit
  pure $ HandleExpr expr cont

parseDef :: Parser Expr
parseDef = do
  void $ string "def"
  ignored
  name <- parseIdent
  void $ string "="
  ignored
  expr <- parseExpr unit
  pure $ DefExpr name expr

parseAssoc :: Parser Assoc
parseAssoc = string "left" *> pure Left <|> string "right" *> pure Right

parseInfix :: Parser Expr
parseInfix = do
  void $ string "infix"
  ignored
  assoc <- parseAssoc
  ignored
  op <- parseOp
  int <- parseInt
  void $ string "="
  ignored
  expr <- parseExpr unit
  pure $ InfixExpr assoc op int expr

parseExtern :: Parser Expr
parseExtern = do
  void $ string "extern"
  ignored
  name <- parseIdent
  pure $ ExternExpr name

parseExpr :: Unit -> Parser Expr
parseExpr unit = parseBlock <|> parseDo <|> parseHandle <|> parseDef <|> parseInfix <|> parseExtern <|> try parseLambda <|> parseOperators unit

parseRepl :: Parser (Maybe Expr)
parseRepl = do
  ignored
  expr <- option Nothing $ Just <$> parseExpr unit
  eof
  pure $ expr

runParseRepl :: String -> Either ParseError (Maybe Expr)
runParseRepl input = runParser input parseRepl