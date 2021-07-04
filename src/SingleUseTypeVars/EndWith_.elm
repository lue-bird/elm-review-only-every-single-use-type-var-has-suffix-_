module SingleUseTypeVars.EndWith_ exposing (rule)

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


{-| Reports single-use type variables that don't have a -\_ suffix.

    config =
        [ SingleUseTypeVars.EndWith_.rule
        ]


### Fail

    a : a

    type Id phantomTag
        = Id Int


### Success

    a : a_

    type Id phantomTag_
        = Id Int


## Don't use this

When you already use the -\_ suffix in (multi-use) type variables.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "SingleUseTypeVars.EndWith_" ()
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
            (Node.value >> (not << String.endsWith "_"))
        |> groupTypeVars (.length >> (==) 1)


toError :
    List (Node String)
    -> { typeVarName : String, range : Range }
    -> Rule.Error {}
toError typeVars { typeVarName, range } =
    let
        typeVarName_AleadyExist =
            typeVars
                |> List.map Node.value
                |> List.member (typeVarName ++ "_")
    in
    Rule.errorWithFix
        { message =
            "The type variable "
                ++ typeVarName
                ++ " isn't marked as single-use with a -_ suffix."
        , details =
            [ if typeVarName_AleadyExist then
                [ "Choose a different name ("
                , typeVarName
                , "_ already exists)."
                , " Then add the -_ suffix."
                ]
                    |> String.concat

              else
                "Add the -_ suffix."
            ]
        }
        range
        (if typeVarName_AleadyExist then
            []

         else
            [ Fix.insertAt range.end "_" ]
        )
