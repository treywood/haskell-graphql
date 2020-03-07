module Main where

import GraphQL.Core

data Person = Person { name :: String, age :: Int }

characterType :: SchemaType Person
characterType = ObjectType "Person"
                  [ Field "name" StringType (return . name)
                  , Field "age" IntType (return . age)
                  ]

trey = Person { name = "Trey", age = 31 }
debbie = Person { name = "Debbie", age = 32 }

queryType :: SchemaType ()
queryType = ObjectType "Query"
                [ Field "people" (ListType characterType) (\() -> return [trey, debbie])
                ]

myQuery = [ Query "people" [ Query "name" [], Query "age" [] ] ]

main :: IO ()
main = do
    result <- runQuery queryType myQuery ()
    putStrLn (show result)