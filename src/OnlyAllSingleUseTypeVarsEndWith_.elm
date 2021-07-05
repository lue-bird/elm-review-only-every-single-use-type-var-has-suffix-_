module OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import List.Extra as List
import List.NonEmpty
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import SyntaxHelp exposing (collectLetDeclarationsFromExpression, collectTypeVarsFromDeclaration, collectTypeVarsFromType, collectTypesFromLetDeclaration)


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
    collectTypeVarsFromDeclaration declaration
        |> checkTypeVarsForErrors


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor expression =
    collectLetDeclarationsFromExpression expression
        |> List.concatMap collectTypesFromLetDeclaration
        |> List.concatMap collectTypeVarsFromType
        |> checkTypeVarsForErrors


checkTypeVarsForErrors : List (Node String) -> List (Rule.Error {})
checkTypeVarsForErrors typeVars =
    [ let
        isSingleUse list =
            List.NonEmpty.length list == 1

        singleUseTypeVarsThatDontEndWith_ =
            typeVars
                |> List.filter
                    ((not << String.endsWith "_") << Node.value)
                |> groupBy Node.value
                |> List.filter isSingleUse
                |> List.map List.NonEmpty.head
      in
      singleUseTypeVarsThatDontEndWith_
        |> List.map (singleUseTypeVarDoesntEndWith_Error typeVars)
    , let
        isMultiUse list =
            List.NonEmpty.length list >= 2

        multiUseTypeVarsThatEndWith_ =
            typeVars
                |> List.filter
                    (String.endsWith "_" << Node.value)
                |> groupBy Node.value
                |> List.filter isMultiUse
      in
      multiUseTypeVarsThatEndWith_
        |> List.map (multiUseTypeVarsEndWith_Error typeVars)
    ]
        |> List.concat


singleUseTypeVarDoesntEndWith_Error : List (Node String) -> Node String -> Rule.Error {}
singleUseTypeVarDoesntEndWith_Error allTypeVars culprit =
    let
        typeVarName =
            Node.value culprit

        typeVarName_AleadyExist =
            allTypeVars
                |> List.map Node.value
                |> List.member (typeVarName ++ "_")
    in
    Rule.errorWithFix
        { message =
            [ "The type variable "
            , typeVarName
            , " isn't marked as single-use with a -_ suffix."
            ]
                |> String.concat
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
        (Node.range culprit)
        (if typeVarName_AleadyExist then
            []

         else
            [ Fix.insertAt (Node.range culprit).end "_" ]
        )


multiUseTypeVarsEndWith_Error :
    List (Node String)
    -> List.NonEmpty.NonEmpty (Node String)
    -> Rule.Error {}
multiUseTypeVarsEndWith_Error allTypeVars culprits =
    let
        culpritsName =
            Node.value (List.NonEmpty.head culprits)

        culpritsRange =
            culprits
                |> List.NonEmpty.map Node.range
                |> List.NonEmpty.toList
                |> Range.combine

        typeVarNameWithout_AleadyExist =
            allTypeVars
                |> List.map Node.value
                |> List.member (culpritsName |> String.dropRight 1)
    in
    Rule.errorWithFix
        { message =
            [ "The type variable "
            , culpritsName
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
                    , culpritsName |> String.dropRight 1
                    , " already exists)."
                    ]
                        |> String.concat

                else
                    "remove the -_ suffix (there's a fix available for that)."
              ]
                |> String.concat
            ]
        }
        culpritsRange
        (if typeVarNameWithout_AleadyExist then
            []

         else
            culprits
                |> List.NonEmpty.map
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
                |> List.NonEmpty.toList
        )


biggestReasonOfExistance : String
biggestReasonOfExistance =
    "-_ at the end of a type variable is a good indication that it is used only in this one place."



-- util


groupBy : (a -> comparable_) -> List a -> List ( a, List a )
groupBy toComparable list =
    list
        |> List.sortBy toComparable
        |> List.groupWhile
            (\a b -> toComparable a == toComparable b)
