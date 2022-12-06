module Main where

import Control.Monad (forever)
import GraphQL.Core (runQuery)
import GraphQL.Parser (parseQuery)
import GraphQL.Schema (
    Field (..),
    Interface (..),
    SchemaType (..),
    printSchema,
 )
import System.IO
import System.Random

data Gender
    = MALE
    | FEMALE
    deriving (Eq, Show)

data Person = Person
    { name :: String
    , age :: Int
    , gender :: Gender
    , spouse :: Maybe Person
    }
    deriving (Show)

data Specie
    = DOG
    | CAT
    deriving (Eq, Show, Read)

data Pet
    = Pet Specie String
    deriving (Read, Show)

class Named a where
    getName :: a -> String

data NamedType
    = NamedPerson Person
    | NamedPet Pet

instance Named Person where
    getName = name

instance Named Pet where
    getName (Pet _ petName) = petName

instance Named NamedType where
    getName (NamedPerson p) = getName p
    getName (NamedPet p) = getName p

hasNameInterface :: (Named a) => Interface a
hasNameInterface =
    Interface "HasName" [Field "name" StringType [] (return . getName)]

genderType = EnumType "Gender" [MALE, FEMALE]

specieType = EnumType "Specie" [DOG, CAT]

personType :: SchemaType Person
personType =
    ObjectType
        "Person"
        [hasNameInterface]
        [ Field "age" IntType [] (return . age)
        , Field "gender" genderType [] (return . gender)
        , Field "spouse" (NullableType personType) [] (return . spouse)
        ]

petType :: SchemaType Pet
petType =
    ObjectType
        "Pet"
        [hasNameInterface]
        [Field "specie" specieType [] (\(Pet specie _) -> return specie)]

queryType :: SchemaType ()
queryType =
    ObjectType
        "Query"
        []
        [ Field "people" (NullableType (ListType personType)) [] getPeople
        , Field "pets" (ListType petType) [] getPets
        , Field "named" (InterfaceType hasNameInterface) [] getNamed
        ]
  where
    getPets :: () -> IO [Pet]
    getPets () = do
        contents <- readFile "pets.txt"
        return $ map read (lines contents)
    getPeople :: () -> IO (Maybe [Person])
    getPeople () = do
        random <- (randomIO :: IO Float)
        return $
            if random > 0.5
                then Just [trey, debbie]
                else Nothing
    getNamed :: () -> IO NamedType
    getNamed () = do
        random <- (randomIO :: IO Float)
        return $
            if random > 0.5
                then NamedPerson trey
                else NamedPet (Pet DOG "Murphy")
    trey = Person{name = "Trey", age = 31, gender = MALE, spouse = Just debbie}
    debbie =
        Person{name = "Debbie", age = 32, gender = FEMALE, spouse = Just trey}

main :: IO ()
main = do
    putStrLn "\nSchema:\n"
    putStrLn $ printSchema queryType
    putStrLn ""
    forever $ do
        putStr "Query: "
        hFlush stdout
        queryStr <- getLine
        let parseResult = parseQuery queryStr
        case parseResult of
            Right query -> do
                result <- runQuery queryType query ()
                putStrLn $ "Result: " ++ show result
                putStrLn ""
            Left err -> putStrLn err
