module No.MultiUseTypeVars.EndWith_Test exposing (all)

import No.MultiUse.TypeVar.Has.Suffix_ exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "No.MultiUse.TypeVar.Has.Suffix_"
        [ test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
