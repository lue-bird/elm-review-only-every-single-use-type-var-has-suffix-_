module No.MultiUseTypeVars.EndWith_ exposing (rule)

{-|

@docs rule

-}

import Common exposing (collectTypeVarsFromDeclaration, collectTypeVarsFromExpression, groupTypeVars)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
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
    collectTypeVarsFromDeclaration declaration
        |> groupedTypeVarsThatEndWith_
        |> List.map toError


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor expression =
    collectTypeVarsFromExpression expression
        |> groupedTypeVarsThatEndWith_
        |> List.map toError


groupedTypeVarsThatEndWith_ :
    List (Node String)
    -> List { typeVarName : String, range : Range }
groupedTypeVarsThatEndWith_ typeVars =
    typeVars
        |> List.filter
            (Node.value >> String.endsWith "_")
        |> groupTypeVars
            (\{ length } -> length >= 2)


toError : { typeVarName : String, range : Range } -> Rule.Error {}
toError { typeVarName, range } =
    Rule.error
        { message =
            "The type variable "
                ++ typeVarName
                ++ " is used in multiple places,"
                ++ " despite being marked as single-use with the -_ suffix."
        , details = [ "Rename one of them or remove the -_ suffix." ]
        }
        range
