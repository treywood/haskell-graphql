{-# LANGUAGE QuasiQuotes #-}

import Test.HUnit
import Test.HUnit.Base
import Text.RawString.QQ

import GraphQL.Core
import GraphQL.Parser
import GraphQL.Parser.Internal

parserTestCases :: [(String, String, ParseResult [Query])]
parserTestCases =
    [
        ( "Basic Query"
        , "{\n\tfield1\n\tfield2\n}"
        , Right [Query "field1" Nothing [], Query "field2" Nothing []]
        )
    ,
        ( "Nested Fields"
        , "{ field1 { nested1, nested2 } field2 }"
        , Right
            [ Query
                "field1"
                Nothing
                [Query "nested1" Nothing [], Query "nested2" Nothing []]
            , Query "field2" Nothing []
            ]
        )
    ,
        ( "Alias Fields"
        , "{ alias1: field1, alias2: field2 { nested1, nestedAlias: nested2 } }"
        , Right
            [ Query "field1" (Just "alias1") []
            , Query
                "field2"
                (Just "alias2")
                [ Query "nested1" Nothing []
                , Query "nested2" (Just "nestedAlias") []
                ]
            ]
        )
    ,
        ( "Early End of Input"
        , "{ field1 field2"
        , Left
            [r|1:16:
  |
1 | { field1 field2
  |                ^
unexpected end of input
expecting ',', '{', '}', alphanumeric character, or white space
|]
        )
    ,
        ( "Bad Start"
        , "nope"
        , Left
            [r|1:1:
  |
1 | nope
  | ^
unexpected 'n'
expecting '{'
|]
        )
    ,
        ( "Empty String"
        , ""
        , Left
            [r|1:1:
  |
1 | <empty line>
  | ^
unexpected end of input
expecting '{'
|]
        )
    ]

parserTest :: (String, String, ParseResult [Query]) -> Test
parserTest (label, queryString, expected) =
    let testCase =
            TestCase $ do
                let result = parseQuery queryString
                assertEqual label expected result
     in TestLabel label testCase

parserTests = TestList $ map parserTest parserTestCases

main :: IO ()
main = do
    runTestTT parserTests
    putStrLn "Done"
