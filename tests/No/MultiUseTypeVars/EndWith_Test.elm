module No.MultiUseTypeVars.EndWith_Test exposing (all)

import No.MultiUseTypeVars.EndWith_ exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "No.MultiUseTypeVars.EndWith_"
        [ test "error in function"
            (\() ->
                """
module A exposing (..)

a : a_ -> a_
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a_"
                            , under = "a_ -> a_"
                            }
                        ]
            )
        , test "error in typed"
            (\() ->
                """
module A exposing (..)

a : Result (Maybe a_) a_
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a_"
                            , under = "a_) a_"
                            }
                        ]
            )
        , test "error in record"
            (\() ->
                """
module A exposing (..)

a : { a : a_, b : a_ }
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a_"
                            , under = "a_, b : a_"
                            }
                        ]
            )
        , test "error in tuple"
            (\() ->
                """
module A exposing (..)

a : ( a_, a_ )
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a_"
                            , under = "a_, a_"
                            }
                        ]
            )
        , test "error in let"
            (\() ->
                """
module A exposing (..)

a : ()
a =
    let
        b : ( c_, c_ )
        b = b
    in
    ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "c_"
                            , under = "c_, c_"
                            }
                        ]
            )
        , test "error in type"
            (\() ->
                """
module A exposing (..)

type A a_ = A a_
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a"
                            , under = "a"
                            }
                        ]
            )
        ]


error :
    { typeVar : String, under : String }
    -> Review.Test.ExpectedError
error { typeVar, under } =
    Review.Test.error
        { message =
            "The type variable "
                ++ typeVar
                ++ " is used in multiple places,"
                ++ " despite being marked as single-use with the -_ suffix."
        , details = [ "Rename one of them or remove the -_ suffix." ]
        , under = under
        }
