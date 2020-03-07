import Test.HUnit
import Test.HUnit.Base

import GraphQL.Parser
import GraphQL.Core

parserTestCases :: [(String, String, ParseResult [Query])]
parserTestCases =
    [ ( "Basic Query"
      , "{\n\tfield1\n\tfield2\n}"
      , Right [ Query "field1" Nothing [], Query "field2" Nothing [] ]
      )
    , ( "Nested Fields"
      , "{ field1 { nested1, nested2 } field2 }"
      , Right
          [ Query "field1" Nothing
             [ Query "nested1" Nothing []
             , Query "nested2" Nothing []
             ]
          , Query "field2" Nothing []
          ]
      )
    , ( "Alias Fields"
      , "{ alias1: field1, alias2: field2 { nested1, nestedAlias: nested2 } }"
      , Right
          [ Query "field1" (Just "alias1") []
          , Query "field2" (Just "alias2")
            [ Query "nested1" Nothing []
            , Query "nested2" (Just "nestedAlias") []
            ]
          ]
      )
    , ( "Early End of Input"
      , "{ field1 field2"
      , Left "unexpected end of input at position 13"
      )
    , ( "Bad Start"
      , "nope"
      , Left "expected '{', got 'n' at position 0"
      )
    , ( "Empty String"
      , ""
      , Left "expected '{', got end of input at position 0"
      )
    ]

parserTest :: (String, String, ParseResult [Query]) -> Test
parserTest (label, queryString, expected) =
    let
        testCase = TestCase $ do
            let result = parseQuery queryString
            assertEqual label expected result
    in TestLabel label testCase

parserTests = TestList $ map parserTest parserTestCases

main :: IO ()
main = do
    runTestTT parserTests
    putStrLn "Done"
