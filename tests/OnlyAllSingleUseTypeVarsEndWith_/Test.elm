module OnlyAllSingleUseTypeVarsEndWith_.Test exposing (all)

import OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)
import Review.Test
import Test exposing (Test)


all : Test
all =
    Test.describe "OnlyAllSingleUseTypeVarsEndWith_"
        [ Test.describe "single-use type variable without suffix -_ reported"
            [ Test.test "error in function"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : b_ -> ()
a _ = ()
"""
                            ]
                )
            , Test.test "error in complex type"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : Result ({ rec_ | field : Arr (In Char Never) () }) -> String
a = a
"""
                            ]
                )
            , Test.test "let type variable reference is updated"
                (\() ->
                    """
module A exposing (..)

a : Result ({ rec | field : Arr (In Char Never) () }) -> String
a =
    let
        b : rec
        b = b
    in
    a
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ singleUseTypeVarDoesntEndWith_Error
                                { typeVar = "rec"
                                , under = "rec"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 4, column = 15 }
                                    , end = { row = 4, column = 18 }
                                    }
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : Result ({ rec_ | field : Arr (In Char Never) () }) -> String
a =
    let
        b : rec_
        b = b
    in
    a
"""
                            ]
                )
            ]
        , Test.test "single-use type variable without -_ in let not reported"
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
                    |> Review.Test.expectNoErrors
            )
        , Test.test "single-use type variable named keyword with -_ not reported"
            (\() ->
                """
module A exposing (..)

a : type_ -> ()
a _ = ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "multi-use type variables with suffix -_ in let not reported"
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
                    |> Review.Test.expectNoErrors
            )
        , Test.test "multi-use type variables named keyword with suffix -_ not reported"
            (\() ->
                """
module A exposing (..)

a : ( type_, type_ ) -> ()
a _ =
    ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
            )
        , Test.describe "multi-use type variables with suffix -_ reported"
            [ Test.test "error in function"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : a -> a
a = a
"""
                            ]
                )
            , Test.test "error in typed"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : Result (Maybe a) a
a = a
"""
                            ]
                )
            , Test.test "error in record"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : { a : a, b : a }
a = a
"""
                            ]
                )
            , Test.test "error in tuple"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : ( a, a )
a = a
"""
                            ]
                )
            , Test.test "error in complex type"
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
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : Result ({ a | field : Arr (In a Never) () }) -> ( Float, a, Int )
a = a
"""
                            ]
                )
            , Test.test "let type variable reference is updated"
                (\() ->
                    """
module A exposing (..)

a : Result ({ a_ | field : Arr (In a_ Never) () }) -> ( Float, a_, Int )
a =
    let
        b : a_
        b = b
    in
    a
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ multiUseTypeVarsEndWith_Error
                                { typeVar = "a_"
                                , under = "a_ | field : Arr (In a_ Never) () }) -> ( Float, a_"
                                }
                                |> Review.Test.whenFixed
                                    """
module A exposing (..)

a : Result ({ a | field : Arr (In a Never) () }) -> ( Float, a, Int )
a =
    let
        b : a
        b = b
    in
    a
"""
                            ]
                )
            ]
        , Test.test "complex type with both errors"
            (\() ->
                """
module A exposing (..)

a : ( Task x_ (Typed (Checked (Public a_))), Maybe yay, Result x_ )
a = a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ singleUseTypeVarDoesntEndWith_Error
                            { typeVar = "yay"
                            , under = "yay"
                            }
                            |> Review.Test.whenFixed
                                """
module A exposing (..)

a : ( Task x_ (Typed (Checked (Public a_))), Maybe yay_, Result x_ )
a = a
"""
                        , multiUseTypeVarsEndWith_Error
                            { typeVar = "x_"
                            , under = "x_ (Typed (Checked (Public a_))), Maybe yay, Result x_"
                            }
                            |> Review.Test.whenFixed
                                """
module A exposing (..)

a : ( Task x (Typed (Checked (Public a_))), Maybe yay, Result x )
a = a
"""
                        ]
            )
        ]


singleUseTypeVarDoesntEndWith_Error :
    { typeVar : String, under : String }
    -> Review.Test.ExpectedError
singleUseTypeVarDoesntEndWith_Error info =
    Review.Test.error
        { message =
            "The type variable `"
                ++ info.typeVar
                ++ "` isn't marked as single-use with a -_ suffix."
        , details =
            [ "-_ at the end of a type variable is a good indication that it is used only in this one place."
            , "Add the -_ suffix (there's a fix available for that)."
            ]
        , under = info.under
        }


multiUseTypeVarsEndWith_Error :
    { typeVar : String, under : String }
    -> Review.Test.ExpectedError
multiUseTypeVarsEndWith_Error info =
    Review.Test.error
        { message =
            "The type variable `"
                ++ info.typeVar
                ++ "` is used in multiple places,"
                ++ " despite being marked as single-use with the -_ suffix."
        , details =
            [ "-_ at the end of a type variable is a good indication that it is used only in this one place."
            , "Rename one of them if this was an accident. "
            , "If it wasn't an accident, remove the -_ suffix (there's a fix available for that)."
            ]
        , under = info.under
        }
