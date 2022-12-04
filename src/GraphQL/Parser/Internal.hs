module GraphQL.Parser.Internal (
  parseFieldAndAlias,
  parseJustField,
  ParseResult,
  Parser,
  queryParser,
) where

import GraphQL.Core (Query (..))

import Data.Maybe
import Data.Void
import Relude.Monad.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type ParseResult = Either String

parseFieldAndAlias :: Parser (String, Maybe String)
parseFieldAndAlias =
  try $ do
    alias <- some alphaNumChar
    char ':' >> space
    field <- some alphaNumChar
    return (field, Just alias)

parseJustField :: Parser (String, Maybe String)
parseJustField = do
  field <- some alphaNumChar
  return (field, Nothing)

queryParser :: Parser [Query]
queryParser = do
  char '{' >> space
  queries <-
    some $ do
      (field, alias) <- parseFieldAndAlias <|> parseJustField
      space
      subQueries <- optional . try $ queryParser
      optional (char ',') >> space
      return $ Query field alias (subQueries ?: [])
  space >> char '}'
  return queries
