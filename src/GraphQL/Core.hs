{-# LANGUAGE GADTs #-}

module GraphQL.Core
    ( SchemaType(..)
    , Field(..)
    , Query(..)
    , runQuery
    ) where

import Data.List (intercalate)

data SchemaType a where
    ObjectType :: String -> [Field a] -> SchemaType a
    StringType :: SchemaType String
    IntType :: SchemaType Int
    FloatType :: SchemaType Float
    IDType :: SchemaType String
    ListType :: SchemaType a -> SchemaType [a]

data Field a where
    Field :: String -> SchemaType b -> (a -> IO b) -> Field a

fields :: SchemaType a -> [Field a]
fields (ObjectType _ fs) = fs
fields _ = []

fieldName :: Field a -> String
fieldName (Field name _ _) = name

data FieldValue =
      ObjectValue [(String, FieldValue)]
    | StringValue String
    | IntValue Int
    | FloatValue Float
    | IDValue String
    | ListValue [FieldValue]

instance Show FieldValue where
    show (StringValue s) = "\"" ++ s ++ "\""
    show (IntValue i) = show i
    show (FloatValue f) = show f
    show (IDValue s) = "\"" ++ s ++ "\""
    show (ListValue xs) = show xs
    show (ObjectValue fs) = "{ " ++ (kvPairs fs) ++ " }"
        where
            kvPairs fs =
                let
                    kvs = map (\(k, v) -> "\"" ++ k ++ "\": " ++ (show v)) fs
                in
                    intercalate ", " kvs

data Query = Query String [Query]

getQueryField :: Query -> String
getQueryField (Query name _) = name

mapValue :: SchemaType a -> a -> FieldValue
mapValue StringType = StringValue
mapValue IntType = IntValue
mapValue FloatType = FloatValue
mapValue IDType = IDValue
mapValue _ = undefined

getValue :: SchemaType a -> [Query] -> IO a -> IO FieldValue
getValue StringType _ = fmap StringValue
getValue IntType _    = fmap IntValue
getValue FloatType _  = fmap FloatValue
getValue IDType _     = fmap IDValue
getValue (ListType (ObjectType _ fields)) queries =
    (>>= (runListFields fields queries))
getValue (ListType innerType) _ =
    (>>= (\xs -> return $ ListValue (map (mapValue innerType) xs)))
getValue (ObjectType _ fields) queries =
    (>>= (runObjectFields fields queries))

runField :: Field a -> [Query] -> a -> IO FieldValue
runField (Field _ fieldType fn) queries ctx = getValue fieldType queries (fn ctx)

runObjectFields :: [Field a] -> [Query] -> a -> IO FieldValue
runObjectFields fields queries ctx = do
    let fieldQueries = [ (f, q) | f <- fields, q <- queries, (fieldName f) == (getQueryField q) ]
    let fieldNames = map (fieldName . fst) fieldQueries
    fieldValues <- sequence $ map (\(f, (Query _ qs)) -> runField f qs ctx) fieldQueries
    return $ ObjectValue (zip fieldNames fieldValues)

runListFields :: [Field a] -> [Query] -> [a] -> IO FieldValue
runListFields fields queries ctxs = do
    values <- sequence $ map (runObjectFields fields queries) ctxs
    return $ ListValue values

runQuery :: SchemaType a -> [Query] -> a -> IO FieldValue
runQuery queryType queries ctx = runObjectFields (fields queryType) queries ctx