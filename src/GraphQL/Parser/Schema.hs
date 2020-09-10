module GraphQL.Parser.Schema where

data Fragment =
  Fragment String Query

data Query =
  Query String
