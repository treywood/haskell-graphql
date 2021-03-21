module GraphQL.Parser
  ( parseQuery
  , ParseResult
  ) where

import           GraphQL.Core         (Query (..))

import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

type ParseResult = Either String

queryParser :: Parser [Query]
queryParser = do
  char '{' >> space
  queries <-
    some $ do
      (field, alias) <- pFieldAndAlias <|> pJustField
      space
      subQueries <- optional . try $ queryParser
      optional (char ',') >> space
      return (Query field alias (fromMaybe [] subQueries))
  space >> char '}'
  return queries

pFieldAndAlias :: Parser (String, Maybe String)
pFieldAndAlias =
  try $ do
    alias <- some alphaNumChar
    char ':' >> space
    field <- some alphaNumChar
    return (field, Just alias)

pJustField :: Parser (String, Maybe String)
pJustField = do
  field <- some alphaNumChar
  return (field, Nothing)

parseQuery :: String -> ParseResult [Query]
parseQuery queryStr =
  case (parse queryParser "" queryStr) of
    Right result -> Right result
    Left err     -> Left (errorBundlePretty err)
