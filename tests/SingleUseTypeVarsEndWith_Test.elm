module SingleUseTypeVarsEndWith_Test exposing (all)

import Review.Test
import SingleUseTypeVarsEndWith_ exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "SingleUseTypeVarsEndWith_"
        [ test "error in function"
            (\() ->
                """
module A exposing (..)

a : b -> ()
a _ = ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "b"
                            , under = "b"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

a : b_ -> ()
a _ = ()
"""
                        ]
            )
        , test "error in let"
            (\() ->
                """
module A exposing (..)

a : ()
a =
    let
        b : c
        b = b
    in
    ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "c"
                            , under = "c"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

a : ()
a =
    let
        b : c_
        b = b
    in
    ()
"""
                        ]
            )
        , test "error in custom type"
            (\() ->
                """
module A exposing (..)

type A a = A
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a"
                            , under = "a"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

type A a_ = A
"""
                        ]
            )
        , test "error in complex type"
            (\() ->
                """
module A exposing (..)

a : Result ({ rec | field : Arr (In Char Never) () }) -> String
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "rec"
                            , under = "rec"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

a : Result ({ rec_ | field : Arr (In Char Never) () }) -> String
a = a
"""
                        ]
            )
        ]


error :
    { typeVar : String, under : String }
    -> Review.Test.ExpectedError
error { typeVar, under } =
    Review.Test.error
        { message =
            [ "The type variable "
            , typeVar
            , " isn't marked as single-use with a -_ suffix."
            ]
                |> String.concat
        , details = [ "Add the -_ suffix (there's a fix available for that)." ]
        , under = under
        }
