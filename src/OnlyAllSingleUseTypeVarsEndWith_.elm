module OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)

{-|

@docs rule

-}

import Common exposing (collectTypeVarsFromDeclaration, listGroupBy, multiUseTypeVarsEndWith_ErrorInfo, singleUseTypeVarDoesntEndWith_ErrorInfo)
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


## Why you might not want this

  - You want to keep the conventional way of naming type variables for consistency?
  - You already use the -\_ suffix in possibly multi-use type variables?
  - You dislike having possibly multi-use -\_ suffixed type variables in your let declarations?

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "OnlyAllSingleUseTypeVarsEndWith_" ()
        |> Rule.withSimpleDeclarationVisitor
            (\(Node _ declaration) ->
                declaration |> declarationCheck
            )
        |> Rule.fromModuleRuleSchema


declarationCheck : Declaration -> List (Rule.Error {})
declarationCheck declaration =
    declaration
        |> collectTypeVarsFromDeclaration
        |> typeVarsCheck


typeVarsCheck :
    { inAnnotation : List (Node String)
    , inLets : List (Node String)
    }
    -> List (Rule.Error {})
typeVarsCheck typeVars =
    let
        ( typeVarsWith_, typeVarsWithout_ ) =
            typeVars.inAnnotation
                |> listGroupBy Node.value
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
                                |> singleUseTypeVarWithout_Error typeVars
                            ]

                        ( _, _ :: _ ) ->
                            []
                )
        , typeVarsWith_
            |> List.filter
                (\occurrences ->
                    List.NonEmpty.length occurrences >= 2
                )
            |> List.map (multiUseTypeVarsWith_Error typeVars)
        ]


singleUseTypeVarWithout_Error :
    { inAnnotation : List (Node String)
    , inLets : List (Node String)
    }
    -> Node String
    -> Rule.Error {}
singleUseTypeVarWithout_Error typeVars typeVarNode =
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
            typeVars.inLets
                |> List.filter (\(Node _ typeVarInLet) -> typeVarInLet == typeVar)
                |> (::) typeVarNode
                |> List.concatMap
                    (\(Node typeVarInLetRange _) ->
                        [ Fix.insertAt typeVarInLetRange.end "_" ]
                    )
        )


multiUseTypeVarsWith_Error :
    { inAnnotation : List (Node String)
    , inLets : List (Node String)
    }
    -> List.NonEmpty.NonEmpty (Node String)
    -> Rule.Error {}
multiUseTypeVarsWith_Error typeVars culprits =
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
            List.concat
                [ culprits |> List.NonEmpty.toList
                , typeVars.inLets
                    |> List.filter
                        (\(Node _ typeVarInLet) ->
                            typeVarInLet == typeVar
                        )
                ]
                |> List.map
                    (\(Node { end } _) ->
                        Fix.removeRange
                            { start = { end | column = end.column - 1 }
                            , end = end
                            }
                    )
        )
