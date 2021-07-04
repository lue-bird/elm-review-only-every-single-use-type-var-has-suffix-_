module No.MultiUseTypeVars.EndWith_ exposing (rule)

{-|

@docs rule

-}

import Common exposing (collectTypeVarsFromDeclaration, collectTypeVarsFromExpression, groupTypeVars)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports multi-use type variables that have a -\_ suffix.

    config =
        [ No.MultiUseTypeVars.EndWith_.rule
        ]


### Fail

    a : a_ -> a_

    type Id value_
        = Id value_


### Success

    a : a -> a

    type Id value
        = Id value


## Don't use this

When you already use the -\_ suffix in (multi-use) type variables.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "No.MultiUseTypeVars.EndWith_" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Rule.Error {})
declarationVisitor declaration =
    let
        typeVars =
            collectTypeVarsFromDeclaration declaration
    in
    typeVars
        |> groupedTypeVarsThatEndWith_
        |> List.map (toError typeVars)


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor expression =
    let
        typeVars =
            collectTypeVarsFromExpression expression
    in
    typeVars
        |> groupedTypeVarsThatEndWith_
        |> List.map (toError typeVars)


groupedTypeVarsThatEndWith_ :
    List (Node String)
    -> List { typeVarName : String, range : Range }
groupedTypeVarsThatEndWith_ typeVars =
    typeVars
        |> List.filter
            (Node.value >> String.endsWith "_")
        |> groupTypeVars
            (\{ length } -> length >= 2)


toError :
    List (Node String)
    -> { typeVarName : String, range : Range }
    -> Rule.Error {}
toError typeVars { typeVarName, range } =
    let
        typeVarNameWithout_AleadyExist =
            typeVars
                |> List.map Node.value
                |> List.member (typeVarName ++ "_")
    in
    Rule.errorWithFix
        { message =
            "The type variable "
                ++ typeVarName
                ++ " is used in multiple places,"
                ++ " despite being marked as single-use with the -_ suffix."
        , details =
            "Rename one of them if this was an accident."
                :: [ [ "If it wasn't an accident, "
                     , if typeVarNameWithout_AleadyExist then
                        [ "choose a different name ("
                        , typeVarName |> String.dropRight 1
                        , " already exists)."
                        ]
                            |> String.concat

                       else
                        "remove the -_ suffix (there's a fix available for that)."
                     ]
                        |> String.concat
                   ]
        }
        range
        (if typeVarNameWithout_AleadyExist then
            []

         else
            typeVars
                |> List.map
                    (\typeVar ->
                        let
                            { start, end } =
                                Node.range typeVar

                            locationOf_ =
                                { row = end.row
                                , column = end.column - 1
                                }
                        in
                        Fix.removeRange
                            { start = locationOf_, end = end }
                    )
        )
