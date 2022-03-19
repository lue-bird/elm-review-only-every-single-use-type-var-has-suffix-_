module OnlyAllSingleUseTypeVarsEndWith_.Test exposing (all)

import Common exposing (multiUseTypeVarsEndWith_ErrorInfo, singleUseTypeVarDoesntEndWith_ErrorInfo)
import OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "OnlyAllSingleUseTypeVarsEndWith_"
        [ describe "single-use type variable without suffix -_"
            [ describe "report" reportSingleUseTypeVarsWithout_
            , describe "unreported" unreportedSingleUseTypeVarsWithout_
            ]
        , describe "multi-use type variables with suffix _"
            [ describe "report" reportMultiUseTypeVarsWith_
            , describe "unreported" unreportedMultiUseTypeVarsWith_
            ]
        , complexTypeWithBothErrors
        ]


unreportedMultiUseTypeVarsWith_ : List Test
unreportedMultiUseTypeVarsWith_ =
    [ test "multi-use -_ in let not reported"
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
    ]


unreportedSingleUseTypeVarsWithout_ : List Test
unreportedSingleUseTypeVarsWithout_ =
    [ test "single-use without -_ in let not reported"
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
    ]


complexTypeWithBothErrors : Test
complexTypeWithBothErrors =
    test "complex type with both errors"
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
                        |> Review.Test.whenFixed """
module A exposing (..)

a : ( Task x_ (Typed (Checked (Public a_))), Maybe yay_, Result x_ )
a = a
"""
                    , multiUseTypeVarsEndWith_Error
                        { typeVar = "x_"
                        , under = "x_ (Typed (Checked (Public a_))), Maybe yay, Result x_"
                        }
                        |> Review.Test.whenFixed """
module A exposing (..)

a : ( Task x (Typed (Checked (Public a_))), Maybe yay, Result x )
a = a
"""
                    ]
        )


reportSingleUseTypeVarsWithout_ : List Test
reportSingleUseTypeVarsWithout_ =
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
    let
        { message, details } =
            singleUseTypeVarDoesntEndWith_ErrorInfo
                { typeVar = typeVar, typeVar_Exists = False }
    in
    Review.Test.error
        { message = message
        , details = details
        , under = under
        }


reportMultiUseTypeVarsWith_ : List Test
reportMultiUseTypeVarsWith_ =
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
    let
        { message, details } =
            multiUseTypeVarsEndWith_ErrorInfo
                { typeVar = typeVar, typeVarWithout_Exists = False }
    in
    Review.Test.error
        { message = message
        , details = details
        , under = under
        }
