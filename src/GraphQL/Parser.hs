module GraphQL.Parser (
    parseQuery,
) where

import Data.Either.Combinators
import GraphQL.Core
import GraphQL.Parser.Internal
import Text.Megaparsec

parseQuery :: String -> ParseResult [Query]
parseQuery = mapLeft errorBundlePretty . parse queryParser ""
