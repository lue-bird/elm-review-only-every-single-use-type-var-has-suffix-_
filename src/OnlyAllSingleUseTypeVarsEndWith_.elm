module OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)

{-|

@docs rule

-}

import Common exposing (collectTypeVarsFromDeclaration, groupBy, multiUseTypeVarsEndWith_ErrorInfo, singleUseTypeVarDoesntEndWith_ErrorInfo)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import List.NonEmpty
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports in types of module scope declarations

  - single-use type variables that don't have a -\_ suffix
  - multi-use type variables that have a -\_ suffix

```
config =
    [ OnlyAllSingleUseTypeVarsEndWith_.rule
    ]
```


### Fail

    empty : List element

    drop : Int -> List element_ -> List element_


### Success

    empty : List element_

    drop : Int -> List element -> List element

y


## Why you might not want this

  - You want to keep the conventional way of naming type variables for consistency?
  - You already use the -\_ suffix in possibly multi-use type variables?
  - You dislike having possibly multi-use -\_ suffixed type variables in your let declarations?

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "OnlyAllSingleUseTypeVarsEndWith_" ()
        |> Rule.withSimpleDeclarationVisitor
            (declarationCheck << Node.value)
        |> Rule.fromModuleRuleSchema


declarationCheck : Declaration -> List (Rule.Error {})
declarationCheck declaration =
    declaration
        |> collectTypeVarsFromDeclaration
        |> checkTypeVars


checkTypeVars :
    { inAnnotation : List (Node String)
    , inLets : List (Node String)
    }
    -> List (Rule.Error {})
checkTypeVars typeVars =
    let
        counts test =
            \occurrences ->
                List.NonEmpty.length occurrences |> test

        ( typeVarsWith_, typeVarsWithout_ ) =
            typeVars.inAnnotation
                |> groupBy Node.value
                |> List.partition
                    (\( Node _ typeVarName, _ ) ->
                        typeVarName |> String.endsWith "_"
                    )
    in
    List.concat
        [ typeVarsWithout_
            |> List.concatMap
                (\typeVarOccurrencesWithout_ ->
                    case typeVarOccurrencesWithout_ of
                        ( onlyTypeVarOccurrenceWithout_, [] ) ->
                            [ onlyTypeVarOccurrenceWithout_
                                |> singleUseTypeVarDoesntEndWith_Error typeVars
                            ]

                        ( _, _ :: _ ) ->
                            []
                )
        , typeVarsWith_
            |> List.filter (counts (\n -> n >= 2))
            |> List.map (multiUseTypeVarsEndWith_Error typeVars)
        ]


singleUseTypeVarDoesntEndWith_Error :
    { inAnnotation : List (Node String)
    , inLets : List (Node String)
    }
    -> Node String
    -> Rule.Error {}
singleUseTypeVarDoesntEndWith_Error typeVars typeVarNode =
    let
        (Node typeVarRange typeVar) =
            typeVarNode

        typeVar_Exists =
            List.concat
                [ typeVars.inAnnotation, typeVars.inLets ]
                |> List.map Node.value
                |> List.member (typeVar ++ "_")
    in
    Rule.errorWithFix
        (singleUseTypeVarDoesntEndWith_ErrorInfo
            { typeVar = typeVar, typeVar_Exists = typeVar_Exists }
        )
        typeVarRange
        (if typeVar_Exists then
            []

         else
            [ Fix.insertAt typeVarRange.end "_" ]
        )


multiUseTypeVarsEndWith_Error :
    { inAnnotation : List (Node String)
    , inLets : List (Node String)
    }
    -> List.NonEmpty.NonEmpty (Node String)
    -> Rule.Error {}
multiUseTypeVarsEndWith_Error typeVars culprits =
    let
        typeVar =
            culprits |> List.NonEmpty.head |> Node.value

        culpritsRange =
            culprits
                |> List.NonEmpty.map Node.range
                |> List.NonEmpty.toList
                |> Range.combine

        typeVarWithout_Exists =
            List.concat
                [ typeVars.inAnnotation, typeVars.inLets ]
                |> List.map Node.value
                |> List.member (typeVar |> String.dropRight 1)
    in
    Rule.errorWithFix
        (multiUseTypeVarsEndWith_ErrorInfo
            { typeVar = typeVar, typeVarWithout_Exists = typeVarWithout_Exists }
        )
        culpritsRange
        (if typeVarWithout_Exists then
            []

         else
            culprits
                |> List.NonEmpty.map
                    (\(Node { end } _) ->
                        Fix.removeRange
                            { start = { end | column = end.column - 1 }
                            , end = end
                            }
                    )
                |> List.NonEmpty.toList
        )
