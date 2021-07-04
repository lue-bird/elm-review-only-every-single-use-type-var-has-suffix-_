module NoMultiUseTypeVarsEndWith_Test exposing (all)

import NoMultiUseTypeVarsEndWith_ exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMultiUseTypeVarsEndWith_"
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
                            |> Review.Test.whenFixed """
module A exposing (..)

a : a -> a
a = a
"""
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
                            |> Review.Test.whenFixed """
module A exposing (..)

a : Result (Maybe a) a
a = a
"""
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
                            |> Review.Test.whenFixed """
module A exposing (..)

a : { a : a, b : a }
a = a
"""
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
                            |> Review.Test.whenFixed """
module A exposing (..)

a : ( a, a )
a = a
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
                            |> Review.Test.whenFixed """
module A exposing (..)

a : ()
a =
    let
        b : ( c, c )
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

type A a_ = A a_
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a_"
                            , under = "a_ = A a_"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

type A a = A a
"""
                        ]
            )
        , test "error in complex type"
            (\() ->
                """
module A exposing (..)

a : Result ({ a_ | field : Arr (In a_ Never) () }) -> ( Float, a_, Int )
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error
                            { typeVar = "a_"
                            , under = "a_ | field : Arr (In a_ Never) () }) -> ( Float, a_"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

a : Result ({ a | field : Arr (In a Never) () }) -> ( Float, a, Int )
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
            "The type variable "
                ++ typeVar
                ++ " is used in multiple places,"
                ++ " despite being marked as single-use with the -_ suffix."
        , details =
            [ "Rename one of them if this was an accident. "
            , [ "If it wasn't an accident, "
              , "remove the -_ suffix (there's a fix available for that)."
              ]
                |> String.concat
            ]
        , under = under
        }
