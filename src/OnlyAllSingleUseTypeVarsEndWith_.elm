module OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import SyntaxHelp exposing (collectLetDeclarationsFromExpression, collectTypeVarsFromDeclaration, collectTypeVarsFromType, collectTypesFromLetDeclaration, groupTypeVars)


{-| Reports

  - single-use type variables that don't have a -\_ suffix
  - multi-use type variables that have a -\_ suffix

```
config =
    [ OnlyAllSingleUseTypeVarsEndWith_.rule
    ]
```


### Fail

    a : a

    type A a
        = A

    a : a_ -> a_

    type A a_
        = A a_


### Success

    a : a_

    type A a_
        = A

    a : a -> a

    type A a
        = A a


## Don't use this

When you already use the -\_ suffix in (multi-use) type variables.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "OnlyAllSingleUseTypeVarsEndWith_" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Rule.Error {})
declarationVisitor declaration =
    let
        typeVars =
            collectTypeVarsFromDeclaration declaration
    in
    [ typeVars
        |> groupedTypeVarsThatDontEndWith_
        |> List.map (singleUseTypeVarDidntEndWith_Error typeVars)
    , typeVars
        |> groupedTypeVarsThatEndWith_
        |> List.map (multiUseTypeVarsEndWith_Error typeVars)
    ]
        |> List.concat


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor expression =
    let
        typeVars =
            collectLetDeclarationsFromExpression expression
                |> List.concatMap collectTypesFromLetDeclaration
                |> List.concatMap collectTypeVarsFromType
    in
    [ typeVars
        |> groupedTypeVarsThatDontEndWith_
        |> List.map (singleUseTypeVarDidntEndWith_Error typeVars)
    , typeVars
        |> groupedTypeVarsThatEndWith_
        |> List.map (multiUseTypeVarsEndWith_Error typeVars)
    ]
        |> List.concat


groupedTypeVarsThatDontEndWith_ :
    List (Node String)
    -> List { typeVarName : String, range : Range }
groupedTypeVarsThatDontEndWith_ typeVars =
    typeVars
        |> List.filter
            (Node.value >> (not << String.endsWith "_"))
        |> groupTypeVars (.length >> (==) 1)


groupedTypeVarsThatEndWith_ :
    List (Node String)
    -> List { typeVarName : String, range : Range }
groupedTypeVarsThatEndWith_ typeVars =
    typeVars
        |> List.filter
            (Node.value >> String.endsWith "_")
        |> groupTypeVars
            (\{ length } -> length >= 2)


singleUseTypeVarDidntEndWith_Error :
    List (Node String)
    -> { typeVarName : String, range : Range }
    -> Rule.Error {}
singleUseTypeVarDidntEndWith_Error typeVars { typeVarName, range } =
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
            [ biggestReasonOfExistance
            , if typeVarName_AleadyExist then
                [ "Choose a different name ("
                , typeVarName
                , "_ already exists)."
                , " Then add the -_ suffix."
                ]
                    |> String.concat

              else
                "Add the -_ suffix (there's a fix available for that)."
            ]
        }
        range
        (if typeVarName_AleadyExist then
            []

         else
            [ Fix.insertAt range.end "_" ]
        )


multiUseTypeVarsEndWith_Error :
    List (Node String)
    -> { typeVarName : String, range : Range }
    -> Rule.Error {}
multiUseTypeVarsEndWith_Error typeVars { typeVarName, range } =
    let
        typeVarNameWithout_AleadyExist =
            typeVars
                |> List.map Node.value
                |> List.member (typeVarName ++ "_")
    in
    Rule.errorWithFix
        { message =
            [ "The type variable "
            , typeVarName
            , " is used in multiple places,"
            , " despite being marked as single-use with the -_ suffix."
            ]
                |> String.concat
        , details =
            [ biggestReasonOfExistance
            , "Rename one of them if this was an accident. "
            , [ "If it wasn't an accident, "
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
                            { end } =
                                Node.range typeVar
                        in
                        Fix.removeRange
                            { start = { end | column = end.column - 1 }
                            , end = end
                            }
                    )
        )


biggestReasonOfExistance : String
biggestReasonOfExistance =
    "-_ at the end of a type variable is a good indication that it is used only in this one place."
