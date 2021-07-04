module SingleUseTypeVars.EndWith_Test exposing (all)

import Review.Test
import SingleUseTypeVars.EndWith_ exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Every.SingleUse.TypedVar.Has.Suffix_"
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
