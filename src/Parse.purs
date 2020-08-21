module Parse where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (get, lift, put)
import Data.Array (foldl)
import Data.Array as Array
import Data.BigInt (BigInt, fromString)
import Data.Either (Either)
import Data.List (List(..), many, snoc)
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

parseIdent :: Parser String
parseIdent = do
  head <- letter
  tail <- fromCharArray <$> Array.many (letter <|> digit)
  ignored
  pure $ singleton head <> tail

parseParens :: forall a. (Unit -> Parser a) -> Parser a
parseParens parser = do
  void $ char '('
  ignored
  a <- parser unit
  void $ char ')'
  ignored
  pure a

parseAtomic :: Unit -> Parser Expr
parseAtomic unit = 
  IntExpr <$> parseInt <|> 
  CharExpr <$> parseChar <|> 
  StringExpr <$> parseString <|> 
  IdentExpr <$> parseIdent <|> 
  parseParens parseExpr

parseDot :: Parser (Expr -> Expr)
parseDot = do
  void $ char '.'
  ignored
  name <- parseIdent
  pure $ \e -> DotExpr e name

parseApply :: Parser (Expr -> Expr)
parseApply = do 
  void $ char '('
  ignored
  args <- sepBy (parseExpr unit) (char ',' *> ignored)
  void $ char ')'
  ignored
  last <- option Nothing $ Just <$> parseBlock
  pure $ case last of
    Nothing -> \e -> ApplyExpr e args
    Just expr -> \e -> ApplyExpr e $ snoc args expr

parseTrail :: Unit -> Parser Expr
parseTrail unit = do
  expr <- parseAtomic unit
  trails <- many (parseApply <|> parseDot)
  pure $ foldl (\f g -> g f) expr trails

parseOp :: Parser String
parseOp = do
  name <- fromCharArray <$> Array.some (oneOf $ toCharArray "~`!@$%^&*-=+\\|:<>.?/")
  ignored
  if name == "->" || name == "."
    then fail $ "Reserved operator " <> name 
    else pure name

parseOperator :: Parser (Tuple String Expr)
parseOperator = do
  name <- parseOp
  expr <- parseTrail unit
  pure $ Tuple name expr

parseOperators :: Unit -> Parser Expr
parseOperators unit = do
  expr <- parseTrail unit
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

parseArgs :: Parser (List String)
parseArgs = do
  ignored
  sepBy parseIdent (char ',' <* ignored)

parseLambda :: Parser Expr
parseLambda = do
  args <- parseArgs
  void $ string "->"
  ignored
  expr <- parseExpr unit
  pure $ LambdaExpr args expr

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
  clazz <- option Nothing $ Just <$> try (parseIdent <* char '.')
  name <- parseIdent
  maybeArgs <- option Nothing $ Just <$> do
    void $ char '('
    ignored
    args <- parseArgs
    void $ char ')'
    ignored
    pure args
  void $ char '='
  ignored
  expr <- parseExpr unit
  pure $ case maybeArgs of
    Nothing -> DefExpr clazz name expr
    Just args -> DefExpr clazz name $ LambdaExpr args expr

parseAssoc :: Parser Assoc
parseAssoc = string "left" *> pure LeftAssoc <|> string "right" *> pure RightAssoc

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

parseClass :: Parser Expr
parseClass = do
  void $ string "class"
  ignored
  name <- parseIdent
  void $ char '('
  ignored
  args <- sepBy parseIdent (char ',' *> ignored)
  void $ char ')'
  ignored
  pure $ ClassExpr name args

parseExtern :: Parser Expr
parseExtern = do
  void $ string "extern"
  ignored
  name <- parseIdent
  pure $ ExternExpr name

parseExpr :: Unit -> Parser Expr
parseExpr unit = parseBlock <|> parseDo <|> parseHandle <|> parseDef <|> parseInfix <|> parseClass <|> parseExtern <|> try parseLambda <|> parseOperators unit

parseProgram :: Parser (List Expr)
parseProgram = do
  ignored
  results <- many $ parseExpr unit
  eof
  pure results

parseRepl :: Parser (Maybe Expr)
parseRepl = do
  ignored
  expr <- option Nothing $ Just <$> parseExpr unit
  eof
  pure $ expr

runParseRepl :: String -> Either ParseError (Maybe Expr)
runParseRepl input = runParser input parseRepl