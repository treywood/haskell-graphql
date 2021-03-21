module GraphQL.Parser
  ( parseQuery
  , ParseResult
  ) where

import           GraphQL.Core            (Query (..))
import           GraphQL.Parser.Internal

import           Text.Megaparsec

parseQuery :: String -> ParseResult [Query]
parseQuery queryStr =
  case (parse queryParser "" queryStr) of
    Right result -> Right result
    Left err     -> Left (errorBundlePretty err)
