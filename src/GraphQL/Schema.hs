{-# LANGUAGE GADTs #-}

module GraphQL.Schema
  ( SchemaType (..),
    Interface (..),
    Field (..),
    printSchema,
  )
where

import Data.List (intercalate)

unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

data Interface a
  = Interface String [Field a]

data SchemaType a where
  ObjectType :: String -> [Interface a] -> [Field a] -> SchemaType a
  InterfaceType :: Interface a -> SchemaType a
  StringType :: SchemaType String
  BooleanType :: SchemaType Bool
  IntType :: SchemaType Int
  FloatType :: SchemaType Float
  IDType :: SchemaType String
  ListType :: SchemaType a -> SchemaType [a]
  NullableType :: SchemaType a -> SchemaType (Maybe a)
  EnumType :: (Eq a, Show a) => String -> [a] -> SchemaType a

data TypeBox where
  TypeBox :: SchemaType a -> TypeBox

instance Show TypeBox where
  show (TypeBox innerType) = show innerType

instance Show (SchemaType a) where
  show (EnumType name vals) =
    "enum " ++ name ++ " {\n  " ++ intercalate "\n  " (map show vals) ++ "\n}"
  show (ObjectType name interfaces fields) =
    let implements =
          if null interfaces
            then ""
            else
              " implements "
                ++ intercalate
                  " & "
                  (map (\(Interface name _) -> name) interfaces)
     in "type "
          ++ name
          ++ implements
          ++ " {\n  "
          ++ intercalate
            "\n  "
            ( map
                (\(Field name fieldType _) -> name ++ ": " ++ typeName fieldType)
                fields
            )
          ++ "\n}"
  show (InterfaceType (Interface name fields)) =
    "interface "
      ++ name
      ++ " {\n  "
      ++ intercalate
        "\n  "
        ( map
            (\(Field name fieldType _) -> name ++ ": " ++ typeName fieldType)
            fields
        )
      ++ "\n}"
  show schemaType = typeName schemaType

typeName :: SchemaType a -> String
typeName (NullableType innerType) =
  let rawName = typeName innerType
   in take (length rawName - 1) rawName
typeName (ObjectType name interfaces _) = requiredName name
typeName (InterfaceType (Interface name _)) = requiredName name
typeName (EnumType name _) = requiredName name
typeName (ListType innerType) =
  requiredName $ "[" ++ typeName innerType ++ "]"
typeName StringType = requiredName "String"
typeName BooleanType = requiredName "Boolean"
typeName IntType = requiredName "Int"
typeName FloatType = requiredName "Float"
typeName IDType = requiredName "ID"

requiredName :: String -> String
requiredName string = string ++ "!"

printSchema :: SchemaType a -> String
printSchema schema = intercalate "\n\n" allTypeStrings ++ "\n"
  where
    allTypeStrings :: [String]
    allTypeStrings =
      (reverse . unique) $ map show (findTypes (TypeBox schema) [] [])
    findTypes :: TypeBox -> [TypeBox] -> [String] -> [TypeBox]
    findTypes schema types typeNames =
      case schema of
        TypeBox (ObjectType name interfaces fields) ->
          if name `elem` typeNames
            then types
            else
              schema :
              concatMap
                ( \(Field _ fieldType _) ->
                    findTypes (TypeBox fieldType) [] (name : typeNames)
                )
                fields
                ++ map (TypeBox . InterfaceType) interfaces
        TypeBox (InterfaceType (Interface name fields)) ->
          if name `elem` typeNames
            then types
            else
              schema :
              concatMap
                ( \(Field _ fieldType _) ->
                    findTypes (TypeBox fieldType) [] (name : typeNames)
                )
                fields
        TypeBox (EnumType name _) ->
          if name `elem` typeNames
            then types
            else schema : types
        TypeBox (ListType innerType) ->
          findTypes (TypeBox innerType) types typeNames
        TypeBox (NullableType innerType) ->
          findTypes (TypeBox innerType) types typeNames
        _ -> types

data Field a where
  Field :: String -> SchemaType b -> (a -> IO b) -> Field a
