module OnlyAllSingleUseTypeVarsEndWith_Test exposing (all)

import OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "OnlyAllSingleUseTypeVarsEndWith_"
        [ describe "single-use type variables end with _"
            singleUseTypeVarsEndWith_Tests
        , describe "no multi-use type variables end with _"
            noMultiUseTypeVarsEndWith_
        , test "complex type with both errors"
            (\() ->
                """
module A exposing (..)

a =
    let
        b : ( Task x_ (Typed (Checked (Public a_))), Maybe yay, Result x_ )
        b = b
    in
    ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ singleUseTypeVarDoesntEndWith_Error
                            { typeVar = "yay"
                            , under = "yay"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

a =
    let
        b : ( Task x_ (Typed (Checked (Public a_))), Maybe yay_, Result x_ )
        b = b
    in
    ()
"""
                        , multiUseTypeVarsEndWith_Error
                            { typeVar = "x_"
                            , under = "x_ (Typed (Checked (Public a_))), Maybe yay, Result x_"
                            }
                            |> Review.Test.whenFixed """
module A exposing (..)

a =
    let
        b : ( Task x (Typed (Checked (Public a_))), Maybe yay, Result x )
        b = b
    in
    ()
"""
                        ]
            )
        ]


singleUseTypeVarsEndWith_Tests : List Test
singleUseTypeVarsEndWith_Tests =
    [ test "error in function"
        (\() ->
            """
module A exposing (..)

a : b -> ()
a _ = ()
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ singleUseTypeVarDoesntEndWith_Error
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
                    [ singleUseTypeVarDoesntEndWith_Error
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
    , test "error in complex type"
        (\() ->
            """
module A exposing (..)

a : Result ({ rec | field : Arr (In Char Never) () }) -> String
a = a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ singleUseTypeVarDoesntEndWith_Error
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


singleUseTypeVarDoesntEndWith_Error :
    { typeVar : String, under : String }
    -> Review.Test.ExpectedError
singleUseTypeVarDoesntEndWith_Error { typeVar, under } =
    Review.Test.error
        { message =
            [ "The type variable "
            , typeVar
            , " isn't marked as single-use with a -_ suffix."
            ]
                |> String.concat
        , details =
            [ biggestReasonOfExistence
            , "Add the -_ suffix (there's a fix available for that)."
            ]
        , under = under
        }


noMultiUseTypeVarsEndWith_ : List Test
noMultiUseTypeVarsEndWith_ =
    [ test "error in function"
        (\() ->
            """
module A exposing (..)

a : a_ -> a_
a = a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ multiUseTypeVarsEndWith_Error
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
                    [ multiUseTypeVarsEndWith_Error
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
                    [ multiUseTypeVarsEndWith_Error
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
                    [ multiUseTypeVarsEndWith_Error
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
                    [ multiUseTypeVarsEndWith_Error
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
    , test "error in complex type"
        (\() ->
            """
module A exposing (..)

a : Result ({ a_ | field : Arr (In a_ Never) () }) -> ( Float, a_, Int )
a = a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ multiUseTypeVarsEndWith_Error
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


multiUseTypeVarsEndWith_Error :
    { typeVar : String, under : String }
    -> Review.Test.ExpectedError
multiUseTypeVarsEndWith_Error { typeVar, under } =
    Review.Test.error
        { message =
            "The type variable "
                ++ typeVar
                ++ " is used in multiple places,"
                ++ " despite being marked as single-use with the -_ suffix."
        , details =
            [ biggestReasonOfExistence
            , "Rename one of them if this was an accident. "
            , [ "If it wasn't an accident, "
              , "remove the -_ suffix (there's a fix available for that)."
              ]
                |> String.concat
            ]
        , under = under
        }


biggestReasonOfExistence : String
biggestReasonOfExistence =
    "-_ at the end of a type variable is a good indication that it is used only in this one place."
