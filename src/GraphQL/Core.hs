{-# LANGUAGE GADTs #-}

module GraphQL.Core
  ( runQuery
  , Query(..)
  ) where

import           Data.List      (intercalate)
import           GraphQL.Schema

data FieldValue
  = ObjectValue [(String, FieldValue)]
  | StringValue String
  | IntValue Int
  | FloatValue Float
  | IDValue String
  | ListValue [FieldValue]
  | MaybeValue (Maybe FieldValue)

instance Show FieldValue where
  show (MaybeValue Nothing) = "null"
  show (MaybeValue (Just innerVal)) = show innerVal
  show (StringValue s) = "\"" ++ s ++ "\""
  show (IntValue i) = show i
  show (FloatValue f) = show f
  show (IDValue s) = "\"" ++ s ++ "\""
  show (ListValue xs) = show xs
  show (ObjectValue fs) = "{ " ++ (kvPairs fs) ++ " }"
    where
      kvPairs fs =
        let kvs = map (\(k, v) -> "\"" ++ k ++ "\": " ++ (show v)) fs
         in intercalate ", " kvs

data Query =
  Query String (Maybe String) [Query]
  deriving (Show, Eq)

fields :: SchemaType a -> [Field a]
fields (ObjectType _ _ fs) = fs
fields _                   = []

fieldName :: Field a -> String
fieldName (Field name _ _) = name

getQueryField :: Query -> String
getQueryField (Query name _ _) = name

getQueryName :: Query -> String
getQueryName (Query _ (Just alias) _) = alias
getQueryName (Query name Nothing _)   = name

getValue :: SchemaType a -> [Query] -> IO a -> IO FieldValue
getValue StringType _ = fmap StringValue
getValue IntType _ = fmap IntValue
getValue FloatType _ = fmap FloatValue
getValue IDType _ = fmap IDValue
getValue (EnumType _ _) _ = fmap (StringValue . show)
getValue (NullableType innerType) queries =
  (>>= (getMaybeValue innerType queries))
getValue (ListType (NullableType innerType)) queries =
  (>>= (\xs ->
          fmap ListValue (sequence $ map (getMaybeValue innerType queries) xs)))
getValue (ListType (ObjectType _ interfaces fields)) queries =
  (>>= (runListFields interfaces fields queries))
getValue (ListType innerType) queries =
  (>>= (\xs ->
          fmap
            ListValue
            (sequence $ map (getValue innerType queries) (map return xs))))
getValue (ObjectType _ interfaces fields) queries =
  (>>= (runObjectFields interfaces fields queries))
getValue (InterfaceType (Interface _ fields)) queries =
  (>>= (runObjectFields [] fields queries))

getMaybeValue :: SchemaType a -> [Query] -> Maybe a -> IO FieldValue
getMaybeValue _ _ Nothing = return $ MaybeValue Nothing
getMaybeValue (ObjectType _ interfaces fields) queries (Just ctx) =
  fmap (MaybeValue . Just) (runObjectFields interfaces fields queries ctx)
getMaybeValue schemaType queries (Just ctx) =
  fmap (MaybeValue . Just) (getValue schemaType queries (return ctx))

runField :: Field a -> [Query] -> a -> IO FieldValue
runField (Field _ fieldType fn) queries ctx =
  getValue fieldType queries (fn ctx)

runObjectFields :: [Interface a] -> [Field a] -> [Query] -> a -> IO FieldValue
runObjectFields interfaces fields queries ctx = do
  let allFields = fields ++ (concat $ map (\(Interface _ fs) -> fs) interfaces)
  let fieldQueries =
        [ (f, q)
        | f <- allFields
        , q <- queries
        , (fieldName f) == (getQueryField q)
        ]
  let fieldNames = map (getQueryName . snd) fieldQueries
  fieldValues <-
    sequence $ map (\(f, (Query _ _ qs)) -> runField f qs ctx) fieldQueries
  return $ ObjectValue (zip fieldNames fieldValues)

runListFields :: [Interface a] -> [Field a] -> [Query] -> [a] -> IO FieldValue
runListFields interfaces fields queries ctxs = do
  values <- sequence $ map (runObjectFields interfaces fields queries) ctxs
  return $ ListValue values

runQuery :: SchemaType a -> [Query] -> a -> IO FieldValue
runQuery queryType queries ctx =
  runObjectFields [] (fields queryType) queries ctx
